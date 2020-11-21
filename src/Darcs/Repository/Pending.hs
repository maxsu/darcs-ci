-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

module Darcs.Repository.Pending
    ( readPending
    , readTentativePending
    , writeTentativePending
    , siftForPending
    , tentativelyRemoveFromPending
    , tentativelyRemoveFromPW
    , revertPending
    , finalizePending
    , makeNewPending
    , tentativelyAddToPending
    , setTentativePending
    ) where

import Darcs.Prelude

import Control.Applicative
import Control.Exception ( catch, IOException )
import System.Directory ( renameFile )

import Darcs.Patch
    ( PrimOf
    , RepoPatch
    , PrimPatch
    , applyToTree
    , readPatch
    )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Permutations
    ( removeFL
    , commuteWhatWeCanFL
    , commuteWhatWeCanRL
    )
import Darcs.Patch.Prim
    ( PrimSift(siftForPending)
    , PrimCanonize(primDecoalesce)
    )
import Darcs.Patch.Progress (progressFL)
import Darcs.Util.Parser ( Parser )
import Darcs.Patch.Read ( ReadPatch(..), bracketedFL )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor(ForStorage) )
import Darcs.Patch.Show ( displayPatch )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..), FL(..), (+>+), (+>>+), (:>)(..), mapFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), mapSeal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePStart )

import Darcs.Repository.Flags ( UpdatePending (..))
import Darcs.Repository.InternalTypes ( Repository, withRepoLocation, unsafeCoerceT )
import Darcs.Repository.Paths ( pendingPath )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Exception ( catchNonExistence )
import Darcs.Util.Lock  ( writeDocBinFile, removeFileMayNotExist )
import Darcs.Util.Printer ( Doc, ($$), text, vcat, (<+>), renderString )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Tree ( Tree )


newSuffix, tentativeSuffix :: String
newSuffix = ".new"
tentativeSuffix = ".tentative"

-- | Read the contents of pending.
readPending :: RepoPatch p => Repository rt p wR wU wT
            -> IO (Sealed (FL (PrimOf p) wR))
readPending = readPendingFile ""

-- |Read the contents of tentative pending.
readTentativePending :: RepoPatch p => Repository rt p wR wU wT
                     -> IO (Sealed (FL (PrimOf p) wT))
readTentativePending = readPendingFile tentativeSuffix

-- |Read the contents of tentative pending.
readNewPending :: RepoPatch p => Repository rt p wR wU wT
               -> IO (Sealed (FL (PrimOf p) wT))
readNewPending = readPendingFile newSuffix

-- |Read the pending file with the given suffix. CWD should be the repository
-- directory.
readPendingFile :: ReadPatch prim => String -> Repository rt p wR wU wT
                -> IO (Sealed (FL prim wX))
readPendingFile suffix _ =
  do
    let filepath = pendingPath ++ suffix
    raw <- gzReadFilePS filepath
    case readPatch raw of
      Right p -> return (mapSeal unFLM p)
      Left e -> fail $ unlines ["Corrupt pending patch: " ++ show filepath, e]
  `catchNonExistence` Sealed NilFL

-- Wrapper around FL where printed format uses { } except around singletons.
-- Now that the Show behaviour of FL p can be customised (using
-- showFLBehavior (*)), we could instead change the general behaviour of FL Prim;
-- but since the pending code can be kept nicely compartmentalised, it's nicer
-- to do it this way.
-- (*) bf: This function does not exist.
newtype FLM p wX wY = FLM { unFLM :: FL p wX wY }

instance ReadPatch p => ReadPatch (FLM p) where
    readPatch' = mapSeal FLM <$> readMaybeBracketedFL readPatch' '{' '}'

instance ShowPatchBasic p => ShowPatchBasic (FLM p) where
    showPatch f = showMaybeBracketedFL (showPatch f) '{' '}' . unFLM

readMaybeBracketedFL :: (forall wY . Parser (Sealed (p wY))) -> Char -> Char
                     -> Parser (Sealed (FL p wX))
readMaybeBracketedFL parser pre post =
    bracketedFL parser pre post <|> (mapSeal (:>:NilFL) <$> parser)

showMaybeBracketedFL :: (forall wX wY . p wX wY -> Doc) -> Char -> Char
                     -> FL p wA wB -> Doc
showMaybeBracketedFL _ pre post NilFL = text [pre] $$ text [post]
showMaybeBracketedFL printer _ _ (p :>: NilFL) = printer p
showMaybeBracketedFL printer pre post ps = text [pre] $$
                                           vcat (mapFL printer ps) $$
                                           text [post]

-- |Write the contents of tentative pending.
writeTentativePending :: RepoPatch p => Repository rt p wR wU wT
                      -> FL (PrimOf p) wT wY -> IO ()
writeTentativePending = writePendingFile tentativeSuffix

-- |Write the contents of new pending. CWD should be the repository directory.
writeNewPending :: RepoPatch p => Repository rt p wR wU wT
                               -> FL (PrimOf p) wT wP -> IO ()
writeNewPending = writePendingFile newSuffix

-- Write a pending file, with the given suffix. CWD should be the repository
-- directory.
writePendingFile :: ShowPatchBasic prim => String -> Repository rt p wR wU wT
                 -> FL prim wX wY -> IO ()
writePendingFile suffix _ = writePatch name . FLM
  where
    name = pendingPath ++ suffix

writePatch :: ShowPatchBasic p => FilePath -> p wX wY -> IO ()
writePatch f p = writeDocBinFile f $ showPatch ForStorage p <> text "\n"

-- | Remove as much as possible of the given list of prim patches from the
-- pending patch. The "as much as possible" is due to --look-for-* options
-- which cause changes that normally must be explicitly done by the user (such
-- as add, move, and replace) to be inferred from the the diff between
-- pristine and working. These changes cannot be removed from pending because
-- they have never been part of it.
--
-- This function is used by Darcs whenever it adds a patch to the repository
-- (eg. with apply or record). Think of it as one part of transferring patches
-- from pending to somewhere else.
tentativelyRemoveFromPending :: forall rt p wR wU wT wO. RepoPatch p
                             => Repository rt p wR wU wT
                             -> FL (PrimOf p) wO wT
                             -> IO ()
tentativelyRemoveFromPending r ps = do
    Sealed pend <- readTentativePending (unsafeCoerceT r :: Repository rt p wR wU wO)
    Sealed newpend <-
        return $ updatePending (progressFL "Removing from pending:" ps) pend NilFL
    writeTentativePending r newpend

-- | Similar to 'tentativelyRemoveFromPending', but also takes the (old)
-- difference between pending and working into account. It is used by amend and
-- record commands to adjust the pending patch. See the docs for
-- 'updatePending' below for details.
tentativelyRemoveFromPW :: forall rt p wR wO wT wP wU. RepoPatch p
                        => Repository rt p wR wU wT
                        -> FL (PrimOf p) wO wT -- added repo changes
                        -> FL (PrimOf p) wO wP -- O = old tentative
                        -> FL (PrimOf p) wP wU -- P = (old) pending
                        -> IO ()
tentativelyRemoveFromPW r changes pending working = do
    Sealed pending' <- return $
        updatePending (progressFL "Removing from pending:" changes) pending working
    writeTentativePending r pending'

{- |
@'updatePending' changes pending working@ updates @pending@ by removing the
@changes@ we added to the repository. If primitive patches were atomic, we
could assume that @changes@ is a subset of @pending +>+ working@, but alas,
they are not: before we select changes we coalesce them; and during
selection we can again arbitrarily split them (though currently this is
limited to hunks).

The algorithm is as follows. For each @x@ in @changes@ we first try to
remove it from @pending@ as is. If this fails, we commute it past @pending@,
pushing any (reverse) dependencies with it, and check if we can remove the
result from @working@.

If prim patches were atomic this check would always succeed and we would be
done now. But due to coalescing and splitting of prims it can fail, so we
must try harder: we now try to decoalesce the commuted changes from
@working@. If that fails, too, then we know that our @x@ originated from
@pending@. So we backtrack and decoalesce @x@ from @pending@. This final
step must not fail. If it does, then we have a bug because it means we
recorded a change that cannot be removed from the net effect of @pending@
and @working@.
-}
updatePending :: (PrimPatch p)
              => FL p wA wB -> FL p wA wC -> FL p wC wD -> Sealed (FL p wB)
-- no changes to the repo => cancel patches in pending whose inverse are in working
updatePending NilFL ys zs = removeRLFL (reverseFL ys) zs
-- pending is empty => keep it that way
updatePending _ NilFL _ = Sealed NilFL
-- no working changes =>
--  just prepend inverted repo changes and rely on sifting to clean up pending
updatePending xs ys NilFL = Sealed (invert xs +>+ ys)
-- x can be removed from pending => continue with the rest
updatePending (x:>:xs) ys zs | Just ys' <- removeFL x ys = updatePending xs ys' zs
-- x and its reverse dependencies can be commuted through pending
-- *and* the result can be removed or decoalesced from working
updatePending (x:>:xs) ys zs
  | ys' :> ix' :> deps <- commuteWhatWeCanFL (invert x :> ys)
  , Just zs' <- removeFromWorking (invert (ix':>:deps)) zs = updatePending xs ys' zs'
  where
    removeFromWorking as bs = removeAllFL as bs <|> decoalesceAllFL bs as
-- decoalesce x from ys and continue with the rest
updatePending (x:>:xs) ys zs =
  case decoalesceFL ys x of
    Just ys' -> updatePending xs ys' zs
    Nothing ->
      error $ renderString
        $ text "cannot eliminate repo change:"
        $$ displayPatch x
        $$ text "from pending:"
        $$ vcat (mapFL displayPatch ys)
        $$ text "or working:"
        $$ vcat (mapFL displayPatch zs)

-- | Remove as many patches as possible of an 'RL' from an adjacent 'FL'.
removeRLFL :: (Commute p, Invert p, Eq2 p)
           => RL p wA wB -> FL p wB wC -> Sealed (FL p wA)
removeRLFL (ys:<:y) zs
  | Just zs' <- removeFL (invert y) zs = removeRLFL ys zs'
  | otherwise = case commuteWhatWeCanRL (ys :> y) of
      deps :> y' :> ys' -> mapSeal ((deps:<:y') +>>+) $ removeRLFL ys' zs
removeRLFL NilRL _ = Sealed NilFL

-- | Remove all patches of the first 'FL' from the second 'FL' or fail.
removeAllFL :: (Commute p, Invert p, Eq2 p)
            => FL p wA wB -> FL p wA wC -> Maybe (FL p wB wC)
removeAllFL (y:>:ys) zs
  | Just zs' <- removeFL y zs = removeAllFL ys zs'
  | otherwise = Nothing
removeAllFL NilFL zs = Just zs

-- | Decoalesce all patches in the second 'FL' from the first 'FL' or fail.
decoalesceAllFL :: (Commute p, Invert p, PrimCanonize p)
                => FL p wA wC -> FL p wA wB -> Maybe (FL p wB wC)
decoalesceAllFL zs (y:>:ys)
  | Just zs' <- decoalesceFL zs y = decoalesceAllFL zs' ys
  | otherwise = Nothing
decoalesceAllFL zs NilFL = Just zs

-- | Decoalesce (subtract) a single patch from an 'FL' by trying to
-- decoalesce it with every element until it succeeds or we cannot
-- commute it any further.
decoalesceFL :: (Commute p, Invert p, {- Eq2 p,  -}PrimCanonize p)
             => FL p wA wC -> p wA wB -> Maybe (FL p wB wC)
decoalesceFL NilFL y = Just (invert y :>: NilFL)
decoalesceFL (z :>: zs) y
  | Just z' <- primDecoalesce z y = Just (z' :>: zs)
  | otherwise = do
      z' :> iy' <- commute (invert y :> z)
      zs' <- decoalesceFL zs (invert iy')
      return (z' :>: zs')

-- | @makeNewPending repo YesUpdatePending pendPs@ verifies that the
--   @pendPs@ could be applied to pristine if we wanted to, and if so
--   writes it to disk.  If it can't be applied, @pendPs@ must
--   be somehow buggy, so we save it for forensics and crash.
makeNewPending :: (RepoPatch p, ApplyState p ~ Tree)
                 => Repository rt p wR wU wT
                 -> UpdatePending
                 -> FL (PrimOf p) wT wP
                 -> Tree IO  -- ^recorded state of the repository, to check if pending can be applied
                 -> IO ()
makeNewPending _                  NoUpdatePending _ _ = return ()
makeNewPending repo YesUpdatePending origp recordedState =
    withRepoLocation repo $
    do let newname = pendingPath ++ ".new"
       debugMessage $ "Writing new pending:  " ++ newname
       Sealed sfp <- return $ siftForPending origp
       writeNewPending repo sfp
       Sealed p <- readNewPending repo
       -- We don't ever use the resulting tree.
       _ <- catch (applyToTree p recordedState) $ \(err :: IOException) -> do
         let buggyname = pendingPath ++ "_buggy"
         renameFile newname buggyname
         error $ renderString
            $ text ("There was an attempt to write an invalid pending! " ++ show err)
            $$ text "If possible, please send the contents of" <+> text buggyname
            $$ text "along with a bug report."
       renameFile newname pendingPath
       debugMessage $ "Finished writing new pending:  " ++ newname

-- | Replace the pending patch with the tentative pending.
--   If @NoUpdatePending@, this merely deletes the tentative pending
--   without replacing the current one.
--
--   Question (Eric Kow): shouldn't this also delete the tentative
--   pending if @YesUpdatePending@?  I'm just puzzled by the seeming
--   inconsistency of the @NoUpdatePending@ doing deletion, but
--   @YesUpdatePending@ not bothering.
finalizePending :: (RepoPatch p, ApplyState p ~ Tree)
                => Repository rt p wR wU wT
                -> UpdatePending
                -> Tree IO
                -> IO ()
finalizePending repo NoUpdatePending _ =
  withRepoLocation repo $ removeFileMayNotExist pendingPath
finalizePending repo upe@YesUpdatePending recordedState =
  withRepoLocation repo $ do
      Sealed tpend <- readTentativePending repo
      Sealed new_pending <- return $ siftForPending tpend
      makeNewPending repo upe new_pending recordedState

revertPending :: RepoPatch p
              => Repository rt p wR wU wT
              -> UpdatePending
              -> IO ()
revertPending r upe = do
  removeFileMayNotExist (pendingPath ++ ".tentative")
  Sealed x <- readPending r
  if upe == YesUpdatePending
    then writeTentativePending (unsafeCoerceT r) x
    else removeFileMayNotExist pendingPath

-- | @tentativelyAddToPending repo ps@ appends @ps@ to the pending patch.
--
--   This fuction is unsafe because it accepts a patch that works on the
--   tentative pending and we don't currently track the state of the
--   tentative pending.
tentativelyAddToPending :: forall rt p wR wU wT wX wY. RepoPatch p
                        => Repository rt p wR wU wT
                        -> FL (PrimOf p) wX wY
                        -> IO ()
tentativelyAddToPending repo patch =
    withRepoLocation repo $ do
        Sealed pend <- readTentativePending repo
        writeTentativePending repo (pend +>+ unsafeCoercePStart patch)

-- | Overwrites the pending patch with a new one, starting at the tentative state.
setTentativePending :: forall rt p wR wU wT wP. RepoPatch p
                    => Repository rt p wR wU wT
                    -> FL (PrimOf p) wT wP
                    -> IO ()
setTentativePending repo patch = do
    Sealed prims <- return $ siftForPending patch
    withRepoLocation repo $ writeTentativePending repo prims
