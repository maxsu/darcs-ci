--  Copyright (C) 2004,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- |
-- Copyright   : 2004, 2007 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Amend
    (
      amend
    , amendrecord
    ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Data.Maybe ( isNothing, isJust )

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , commandAlias
    , nodefaults
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Util
    ( announceFiles
    , historyEditHelp
    , testTentativeAndMaybeExit
    )
import Darcs.UI.Completion ( modifiedFileArgs, knownFileArgs )
import Darcs.UI.Flags ( diffOpts, pathSetFromArgs )
import Darcs.UI.Options ( (^), oparse, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.PatchHeader ( updatePatchHeader, AskAboutDeps(..)
                            , HijackOptions(..)
                            , runHijackT )

import Darcs.Repository.Flags ( UpdatePending(..), DryRun(NoDryRun) )
import Darcs.Patch ( IsRepoType, RepoPatch, description, PrimOf
                   , effect, invert, invertFL, sortCoalesceFL
                   )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends ( patchSetUnion, findCommonWithThem )
import Darcs.Patch.Info ( isTag )
import Darcs.Patch.Named ( fmapFL_Named )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Set ( Origin, PatchSet, patchSet2RL )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, patchDesc )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Util.Path ( AnchoredPath )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , identifyRepositoryFor
    , ReadingOrWriting(Reading)
    , tentativelyRemovePatches
    , tentativelyAddPatch
    , withManualRebaseUpdate
    , finalizeRepositoryChanges
    , invalidateIndex
    , readPendingAndWorking
    , readRecorded
    , readRepo
    )
import Darcs.Repository.Pending ( tentativelyRemoveFromPW )
import Darcs.Repository.Prefs ( getDefaultRepo )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , selectionConfigPrim
    , runInvertibleSelection
    , withSelectedPatchFromList
    )
import qualified Darcs.UI.SelectChanges as S
    ( PatchSelectionOptions(..)
    )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL, (:>)(..), (+>+)
    , nullFL, reverseRL, reverseFL, mapFL_FL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), FlippedSeal(..) )

import Darcs.Util.English ( anyOfClause, itemizeVertical )
import Darcs.Util.Printer ( Doc, formatWords, putDocLn, text, (<+>), ($$), ($+$) )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Tree( Tree )


amendDescription :: String
amendDescription = "Improve a patch before it leaves your repository."


amendHelp :: Doc
amendHelp =
  formatWords
  [ "Amend updates a \"draft\" patch with additions or improvements,"
  , "resulting in a single \"finished\" patch."
  ]
  $+$ formatWords
  [ "By default `amend` proposes you to record additional changes."
  , "If instead you want to remove changes, use the flag `--unrecord`."
  ]
  $+$ formatWords
  [ "When recording a draft patch, it is a good idea to start the name with"
  , "`DRAFT:`. When done, remove it with `darcs amend --edit-long-comment`."
  , "Alternatively, to change the patch name without starting an editor, "
  , "use the `--name`/`-m` flag:"
  ]
  $+$ text
    "    darcs amend --match 'name \"DRAFT: foo\"' --name 'foo2'"
  $+$ formatWords
  [ "Like `darcs record`, if you call amend with files as arguments,"
  , "you will only be asked about changes to those files.  So to amend a"
  , "patch to foo.c with improvements in bar.c, you would run:"
  ]
  $+$ text
    "    darcs amend --match 'touch foo.c' bar.c"
  $+$ historyEditHelp

data AmendConfig = AmendConfig
    { amendUnrecord :: Bool
    , notInRemote :: [O.NotInRemote]
    , matchFlags :: [O.MatchFlag]
    , testChanges :: O.TestChanges
    , interactive :: Maybe Bool
    , author :: Maybe String
    , selectAuthor :: Bool
    , patchname :: Maybe String
    , askDeps :: Bool
    , askLongComment :: Maybe O.AskLongComment
    , keepDate :: Bool
    , lookfor :: O.LookFor
    , _workingRepoDir :: Maybe String
    , withContext :: O.WithContext
    , diffAlgorithm :: O.DiffAlgorithm
    , verbosity :: O.Verbosity
    , compress :: O.Compression
    , useIndex :: O.UseIndex
    , umask :: O.UMask
    , sse :: O.SetScriptsExecutable
    , useCache :: O.UseCache
    }

amend :: DarcsCommand
amend = DarcsCommand
    {
      commandProgramName          = "darcs"
    , commandName                 = "amend"
    , commandHelp                 = amendHelp
    , commandDescription          = amendDescription
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = ["[FILE or DIRECTORY]..."]
    , commandCommand              = amendCmd
    , commandPrereq               = amInHashedRepository
    , commandCompleteArgs         = fileArgs
    , commandArgdefaults          = nodefaults
    , commandAdvancedOptions      = odesc advancedOpts
    , commandBasicOptions         = odesc basicOpts
    , commandDefaults             = defaultFlags allOpts
    , commandCheckOptions         = ocheck allOpts
    }
  where
    fileArgs fps flags args =
      if (O.amendUnrecord ? flags)
        then knownFileArgs fps flags args
        else modifiedFileArgs fps flags args
    basicOpts
      = O.amendUnrecord
      ^ O.notInRemote
      ^ O.matchOneNontag
      ^ O.testChanges
      ^ O.interactive --True
      ^ O.author
      ^ O.selectAuthor
      ^ O.patchname
      ^ O.askDeps
      ^ O.askLongComment
      ^ O.keepDate
      ^ O.lookfor
      ^ O.repoDir
      ^ O.withContext
      ^ O.diffAlgorithm
    advancedOpts
      = O.compress
      ^ O.useIndex
      ^ O.umask
      ^ O.setScriptsExecutable
    allOpts = withStdOpts basicOpts advancedOpts
    config = oparse (basicOpts ^ O.verbosity ^ advancedOpts ^ O.useCache) AmendConfig
    amendCmd fps flags args = pathSetFromArgs fps args >>= doAmend (config flags)

amendrecord :: DarcsCommand
amendrecord = commandAlias "amend-record" Nothing amend

doAmend :: AmendConfig -> Maybe [AnchoredPath] -> IO ()
doAmend cfg files =
  withRepoLock NoDryRun (useCache cfg) YesUpdatePending (umask cfg) $
      RebaseAwareJob $ \(repository :: Repository rt p wR wU wR) -> do
    patchSet <- readRepo repository
    FlippedSeal patches <- filterNotInRemote cfg repository patchSet
    withSelectedPatchFromList "amend" patches (patchSelOpts cfg) $ \ (_ :> oldp) -> do
        announceFiles (verbosity cfg) files "Amending changes in"
        -- auxiliary function needed because the witness types differ for the isTag case
        pristine <- readRecorded repository
        pending :> working <-
          readPendingAndWorking
            (diffingOpts cfg)
            (O.moves (lookfor cfg))
            (O.replaces (lookfor cfg))
            repository
            files
        let go :: forall wU1 . FL (PrimOf p) wR wU1 -> IO ()
            go NilFL | not (hasEditMetadata cfg) =
              putInfo cfg "No changes!"
            go ch =
              do let selection_config =
                        selectionConfigPrim First "record"
                            (patchSelOpts cfg)
                            --([All,Unified] `intersect` opts)
                            (Just (primSplitter (diffAlgorithm cfg)))
                            files
                            (Just pristine)
                 (chosenPatches :> _) <- runInvertibleSelection ch selection_config
                 addChangesToPatch cfg repository oldp chosenPatches pending working
        if not (isTag (info oldp))
              -- amending a normal patch
           then if amendUnrecord cfg
                   then do let selection_config =
                                  selectionConfigPrim Last "unrecord"
                                      (patchSelOpts cfg)
                                      -- ([All,Unified] `intersect` opts)
                                      (Just (primSplitter (diffAlgorithm cfg)))
                                      files
                                      (Just pristine)
                           (_ :> chosenPrims) <- runInvertibleSelection (effect oldp) selection_config
                           let invPrims = reverseRL (invertFL chosenPrims)
                           addChangesToPatch cfg repository oldp invPrims pending working
                   else go (sortCoalesceFL (pending +>+ working))
              -- amending a tag
           else if hasEditMetadata cfg && isNothing files
                        -- the user is not trying to add new changes to the tag so there is
                        -- no reason to warn.
                   then go NilFL
                        -- the user is trying to add new changes to a tag.
                   else do if hasEditMetadata cfg
                                -- the user already knows that it is possible to edit tag metadata,
                                -- note that s/he is providing editing options!
                             then ePutDocLn "You cannot add new changes to a tag."
                                -- the user may not be aware that s/he can edit tag metadata.
                             else ePutDocLn "You cannot add new changes to a tag, but you are allowed to edit tag's metadata (see darcs help amend)."
                           go NilFL


addChangesToPatch :: forall rt p wR wU wT wX wY wP
                   . (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                  => AmendConfig
                  -> Repository rt p wR wU wT
                  -> PatchInfoAnd rt p wX wT
                  -> FL (PrimOf p) wT wY
                  -> FL (PrimOf p) wT wP
                  -> FL (PrimOf p) wP wU
                  -> IO ()
addChangesToPatch cfg _repository oldp chs pending working =
  if nullFL chs && not (hasEditMetadata cfg)
    then putInfo cfg "You don't want to record anything!"
    else do
      invalidateIndex _repository
      -- If a rebase is in progress, we want to manually update the rebase
      -- state, using the amendments directly as rebase fixups. This is
      -- necessary because otherwise the normal commute rules for the rebase
      -- state will first remove the original patch then add the amended patch,
      -- and this can lead to more conflicts than using the amendment as a fixup
      -- directly. For example, if a rename operation is amended in, the rename
      -- can be propagated to any edits to the file in the rebase state, whereas
      -- a delete then add would just cause a conflict.
      -- 
      -- We can also signal that any explicit dependencies of the old patch
      -- should be rewritten for the new patch using a 'NameFixup'.
      (_repository, (mlogf, newp)) <-
        withManualRebaseUpdate _repository $ \_repository -> do
          -- Note we pass NoUpdatePending here and below when re-adding the
          -- amended patch, and instead fix pending explicitly further below.
          _repository <-
            tentativelyRemovePatches
              _repository
              (compress cfg)
              NoUpdatePending
              (oldp :>: NilFL)
          (mlogf, newp) <-
            runHijackT AlwaysRequestHijackPermission $
            updatePatchHeader
              "amend"
              (if askDeps cfg
                 then AskAboutDeps _repository
                 else NoAskAboutDeps)
              (patchSelOpts cfg)
              (diffAlgorithm cfg)
              (keepDate cfg)
              (selectAuthor cfg)
              (author cfg)
              (patchname cfg)
              (askLongComment cfg)
              (fmapFL_Named effect (hopefully oldp))
              chs
          let fixups =
                mapFL_FL PrimFixup (invert chs) +>+
                NameFixup (Rename (info newp) (info oldp)) :>:
                NilFL
          setEnvDarcsFiles newp
          _repository <-
            tentativelyAddPatch
              _repository
              (compress cfg)
              (verbosity cfg)
              NoUpdatePending
              newp
          return (_repository, fixups, (mlogf, newp))
      let failmsg = maybe "" (\lf -> "\nLogfile left in " ++ lf ++ ".") mlogf
      testTentativeAndMaybeExit
        _repository
        (verbosity cfg)
        (testChanges cfg)
        (sse cfg)
        (isInteractive cfg)
        ("you have a bad patch: '" ++ patchDesc newp ++ "'")
        "amend it"
        (Just failmsg)
      tentativelyRemoveFromPW _repository chs pending working
      _repository <-
        finalizeRepositoryChanges _repository YesUpdatePending (compress cfg)
          `clarifyErrors` failmsg
      case verbosity cfg of
        O.NormalVerbosity -> putDocLn "Finished amending patch."
        O.Verbose -> putDocLn $ "Finished amending patch:" $$ description newp
        _ -> return ()
      setEnvDarcsPatches (newp :>: NilFL)

filterNotInRemote :: (IsRepoType rt, RepoPatch p)
                  => AmendConfig
                  -> Repository rt p wR wU wT
                  -> PatchSet rt p Origin wR
                  -> IO (FlippedSeal (RL (PatchInfoAnd rt p)) wR)
filterNotInRemote cfg repository patchSet = do
    nirs <- mapM getNotInRemotePath (notInRemote cfg)
    if null nirs
      then
        return (FlippedSeal (patchSet2RL patchSet))
      else do
        putInfo cfg $
          "Determining patches not in" <+> anyOfClause nirs $$ itemizeVertical 2 nirs
        Sealed thems <- patchSetUnion `fmap` mapM readNir nirs
        _ :> only_ours <- return $ findCommonWithThem patchSet thems
        return (FlippedSeal (reverseFL only_ours))
  where
    readNir loc = do
      repo <- identifyRepositoryFor Reading repository (useCache cfg) loc
      rps <- readRepo repo
      return (Sealed rps)
    getNotInRemotePath (O.NotInRemotePath p) = return p
    getNotInRemotePath O.NotInDefaultRepo = do
        defaultRepo <- getDefaultRepo
        let err = fail $ "No default push/pull repo configured, please pass a "
                         ++ "repo name to --" ++ O.notInRemoteFlagName
        maybe err return defaultRepo

hasEditMetadata :: AmendConfig -> Bool
hasEditMetadata cfg = isJust (author cfg)
                    || selectAuthor cfg
                    || isJust (patchname cfg)
                    || askLongComment cfg == Just O.YesEditLongComment
                    || askLongComment cfg == Just O.PromptLongComment
                    || askDeps cfg

-- hasEditMetadata []                    = False
-- hasEditMetadata (Author _:_)          = True
-- hasEditMetadata (SelectAuthor:_)      = True
-- hasEditMetadata (LogFile _:_)         = True -- ??? not listed as an option for amend
-- hasEditMetadata (PatchName _:_)       = True
-- hasEditMetadata (EditLongComment:_)   = True
-- hasEditMetadata (PromptLongComment:_) = True
-- hasEditMetadata (AskDeps:_)           = True
-- hasEditMetadata (_:fs)                = hasEditMetadata fs


patchSelOpts :: AmendConfig -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = verbosity cfg
    , S.matchFlags = matchFlags cfg
    , S.interactive = isInteractive cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext cfg
    }

diffingOpts :: AmendConfig -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts cfg = diffOpts (useIndex cfg) (O.adds (lookfor cfg)) O.NoIncludeBoring (diffAlgorithm cfg)

isInteractive :: AmendConfig -> Bool
isInteractive = maybe True id . interactive

putInfo :: AmendConfig -> Doc -> IO ()
putInfo cfg what = unless (verbosity cfg == O.Quiet) $ putDocLn what
