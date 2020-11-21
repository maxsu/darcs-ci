-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
-- Copyright (C) 2009 Petr Rockai
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.


module Darcs.Repository.Merge
    ( tentativelyMergePatches
    , considerMergeToWorking
    ) where

import Darcs.Prelude

import Control.Monad ( when, unless )
import System.Exit ( exitSuccess )
import System.IO.Error
    ( catchIOError
    , ioeGetErrorType
    , isIllegalOperationErrorType
    )

import Darcs.Util.Tree( Tree )
import Darcs.Util.External ( backupByCopying )

import Darcs.Patch
    ( RepoPatch, IsRepoType, PrimOf, merge
    , effect
    , listConflictedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Ident ( merge2FL )
import Darcs.Patch.Named ( patchcontents, anonymous )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully )
import Darcs.Patch.Progress( progressFL )
import Darcs.Patch.Set ( PatchSet, Origin, patchSet2RL )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), Fork(..), (:\/:)(..), (:/\:)(..), (+>+), (+<<+)
    , mapFL_FL, concatFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal )

import Darcs.Repository.Flags
    ( UseIndex
    , ScanKnown
    , AllowConflicts (..)
    , Reorder (..)
    , UpdatePending (..)
    , ExternalMerge (..)
    , Verbosity (..)
    , Compression (..)
    , WantGuiPause (..)
    , DiffAlgorithm (..)
    , LookForMoves(..)
    , LookForReplaces(..)
    )
import Darcs.Repository.Hashed
    ( tentativelyAddPatches_
    , tentativelyRemovePatches_
    , UpdatePristine(..)
    )
import Darcs.Repository.Pristine
    ( applyToTentativePristine
    , ApplyDir(..)
    )
import Darcs.Repository.InternalTypes ( Repository, repoLocation )
import Darcs.Repository.Pending ( setTentativePending )
import Darcs.Repository.Resolution
    ( externalResolution
    , standardResolution
    , StandardResolution(..)
    , announceConflicts
    )
import Darcs.Repository.State ( unrecordedChanges, readUnrecorded )

import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Path ( anchorPath, displayPath )
import Darcs.Util.Progress( debugMessage )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Printer ( redText, vcat )

data MakeChanges = MakeChanges | DontMakeChanges deriving ( Eq )

{- 'tentativelyMergePatches' is not easy to understand by just staring at
the code. So here is an in-depth explanation.

We start out at the state X at which our repo and the their repo deviate,
assuming any patches common to both repos have first been commuted to the
common part before X. So X is the intermediate state that is existentially
hiddden inside the Fork we get passed as argument. R is our recorded state
and Y is the recorded state of their repo.

 Y       R
  \     /
 them  us
    \ /
     X
     |
   common
     |
     O

We will elide the common part from now on. It doesn't change and we only
pass it unmodified to standardResolution, see below.

The easy part is to merge the local patches (us) with the remote ones
(them), giving us them' and us'.

     T
    / \
  us'  them'
  /     \
 Y       R
  \     /
 them  us
    \ /
     X


We can ignore us' and just add them' on top of us (which are already in our
repo), unless --reorder-patches is in effect, in which case we remove us and
then first add them and afterwards us'. The new state on top is T which
stands for the new /tentative/ state i.e. what will become the recorded
state after we finalize our changes.

But we're not done yet: we must also adapt the pending patch and the working
tree. Note that changing the working tree is not done in this procedure, we
merely return a list of prims to apply to working. Let us add the difference
between pristine and working, which we call pw, to the picture.

     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

It is easy to see now that we must merge pw with them', as both start at the
(old) recorded state. This gives us pw' and them''.

         U'
        / \
      pw' them''
      /     \
     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

Since U is our unrecorded state, them'' leads us from our old unrecorded
state to the new one, so this is what we will return (if there are no
conflicts; if there are, see below).

What about the pending patch? It starts at R and goes half-way toward U
since it is a prefix of pw. The new pending should start at T and go
half-way toward the new working state U'. Instead of adapting the old
pending patch, we set the new pending patch to pw', ignoring the old one.
This relies on sifting to commute out and drop the parts that need not be in
the pending patch, which is done when we finalize the tentative changes.

Up to now we did not consider conflicts. Any new conflicts arising from the
merges we made so far must be "resolved", that is, marked for manual
resolution, if possible, or at least reported o the user. We made two
merges, one with us and one with pw. It is important now to realize that our
existing repo, and in particular the sequence us, could already be
conflicted. Our job is to resolve only /new/ conflicts and not any
unresolved conflicts that were already in our repo. So, from the rightmost
branch of our double merge us+>+pw+>+them'', we should /not/ resolve us. And
since the original pw cannot be conflicted (it consists of prim patches
only) we can disregard it. This leaves only them'' which is what we pass to
standardResolution to generate the markup, along with its full context,
consisting of (common +>+ us +>+ pw).

The resulting "resolution" goes on top, leading to our final unrecorded
state U'':

         U''
         |
        res
         |
         U'
        / \
    pw'  them''
      /     \
     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

In case the patches we pull are in conflict with local /unrecorded/ changes
(i.e. pw), we want to warn the user about that and allow them to cancel the
operation. The reason is that it is hard to reconstruct the original
unrecorded changes when they are messed up with conflict resolution markup.
To see if this is the case we check whether pw' has conflicts. As an extra
precaution we backup any conflicted files, so the user can refer to them to
restore things or compare in a diff viewer.

The patches we return are what we need to update U to U'' i.e. them''+>+res.
The new pending patch starts out at the new tentative state, so as explained
above, we set it to pw'+>+res, and again rely on sifting to commute out and
drop anything we don't need.

TODO: We should return a properly coerced @Repository rt p wR wU wT@.
-}

tentativelyMergePatches_ :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                         => MakeChanges
                         -> Repository rt p wR wU wR -> String
                         -> AllowConflicts
                         -> ExternalMerge -> WantGuiPause
                         -> Compression -> Verbosity -> Reorder
                         -> ( UseIndex, ScanKnown, DiffAlgorithm )
                         -> Fork (PatchSet rt p)
                                 (FL (PatchInfoAnd rt p))
                                 (FL (PatchInfoAnd rt p)) Origin wR wY
                         -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches_ mc _repo cmd allowConflicts externalMerge wantGuiPause
  compression verbosity reorder diffingOpts@(useidx, _, dflag) (Fork context us them) = do
    (them' :/\: us')
         <- return $ merge2FL (progressFL "Merging us" us)
                              (progressFL "Merging them" them)
    pw <- unrecordedChanges diffingOpts NoLookForMoves NoLookForReplaces _repo Nothing
    -- Note: we use anonymous here to wrap the unrecorded changes.
    -- This is benign because we only retain the effect of the results
    -- of the merge (pw' and them'').
    anonpw <- n2pia `fmap` anonymous pw
    pw' :/\: them'' <- return $ merge (them' :\/: anonpw :>: NilFL)
    let them''content = concatFL $ progressFL "Examining patches for conflicts" $
                                mapFL_FL (patchcontents . hopefully) them''
    let conflicts =
          standardResolution
            (patchSet2RL context +<<+ us :<: anonpw)
            (reverseFL them'')
    let standard_resolution = mangled conflicts

    debugMessage "Checking for conflicts..."
    when (allowConflicts == YesAllowConflictsAndMark) $
        mapM_ backupByCopying $
        map (anchorPath (repoLocation _repo)) $
        conflictedPaths conflicts

    debugMessage "Announcing conflicts..."
    have_conflicts <-
        announceConflicts cmd allowConflicts externalMerge conflicts

    debugMessage "Checking for unrecorded conflicts..."
    let pw'content = concatFL $ progressFL "Examining patches for conflicts" $
                                mapFL_FL (patchcontents . hopefully) pw'
    case listConflictedFiles pw'content of
        [] -> return ()
        fs -> do
          ePutDocLn $ vcat $ map redText $
            "You have conflicting unrecorded changes to:" : map displayPath fs
          -- we catch "hIsTerminalDevice: illegal operation (handle is closed)"
          -- which can be thrown when we apply patches remotely (i.e. during push)
          confirmed <- promptYorn "Proceed?" `catchIOError` (\e ->
            if isIllegalOperationErrorType (ioeGetErrorType e)
              then return True
              else ioError e)
          unless confirmed $ do
            putStrLn "Cancelled."
            exitSuccess

    debugMessage "Reading working tree..."
    working <- readUnrecorded _repo useidx Nothing

    debugMessage "Working out conflict markup..."
    Sealed resolution <-
        case (externalMerge , have_conflicts) of
            (NoExternalMerge, _)       -> return $ if allowConflicts == YesAllowConflicts
                                                     then seal NilFL
                                                     else standard_resolution
            (_, False)                 -> return $ standard_resolution
            (YesExternalMerge c, True) -> externalResolution dflag working c wantGuiPause
                                             (effect us +>+ pw) (effect them) them''content

    debugMessage "Adding patches to the inventory and writing new pending..."
    when (mc == MakeChanges) $ do
        applyToTentativePristine _repo ApplyNormal verbosity them'
        -- these two cases result in the same trees (that's the idea of
        -- merging), so we only operate on the set of patches and do the
        -- adaption of pristine and pending in the common code below
        _repo <- case reorder of
            NoReorder -> do
                tentativelyAddPatches_ DontUpdatePristine _repo
                    compression verbosity NoUpdatePending them'
            Reorder -> do
                -- we do not actually remove any effect in the end, so
                -- it would be wrong to update the unrevert bundle or
                -- the working tree or pending
                r1 <- tentativelyRemovePatches_ DontUpdatePristineNorRevert _repo
                          compression NoUpdatePending us
                r2 <- tentativelyAddPatches_ DontUpdatePristine r1
                          compression verbosity NoUpdatePending them
                tentativelyAddPatches_ DontUpdatePristine r2
                    compression verbosity NoUpdatePending us'
        setTentativePending _repo (effect pw' +>+ resolution)
    return $ seal (effect them''content +>+ resolution)

tentativelyMergePatches :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                        => Repository rt p wR wU wR -> String
                        -> AllowConflicts
                        -> ExternalMerge -> WantGuiPause
                        -> Compression -> Verbosity -> Reorder
                        -> ( UseIndex, ScanKnown, DiffAlgorithm )
                        -> Fork (PatchSet rt p)
                                (FL (PatchInfoAnd rt p))
                                (FL (PatchInfoAnd rt p)) Origin wR wY
                        -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches = tentativelyMergePatches_ MakeChanges


considerMergeToWorking :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                       => Repository rt p wR wU wR -> String
                       -> AllowConflicts
                       -> ExternalMerge -> WantGuiPause
                       -> Compression -> Verbosity -> Reorder
                       -> ( UseIndex, ScanKnown, DiffAlgorithm )
                       -> Fork (PatchSet rt p)
                               (FL (PatchInfoAnd rt p))
                               (FL (PatchInfoAnd rt p)) Origin wR wY
                       -> IO (Sealed (FL (PrimOf p) wU))
considerMergeToWorking = tentativelyMergePatches_ DontMakeChanges
