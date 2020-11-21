--  Copyright (C) 2003-2005 David Roundy
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

module Darcs.UI.Commands.Unrevert ( unrevert, writeUnrevert ) where

import Darcs.Prelude

import System.Exit ( exitSuccess )
import Darcs.Util.Tree( Tree )

import Darcs.UI.Commands
    ( DarcsCommand(..)
    , withStdOpts
    , nodefaults
    , amInHashedRepository
    , putFinished
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags
    ( diffingOpts, verbosity, useCache, umask, compress, diffAlgorithm
    , isInteractive, withContext )
import Darcs.Repository.Flags
    ( UseIndex(..), ScanKnown (..), Reorder(..), AllowConflicts(..), ExternalMerge(..)
    , WantGuiPause(..), UpdatePending(..), DryRun(NoDryRun) )
import Darcs.UI.Flags ( DarcsFlag )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository ( SealedPatchSet, Repository, withRepoLock, RepoJob(..),
                          considerMergeToWorking,
                          tentativelyAddToPending, finalizeRepositoryChanges,
                          readRepo,
                          readRecorded,
                          applyToWorking, unrecordedChanges )
import Darcs.Repository.Paths ( unrevertPath )
import Darcs.Patch ( IsRepoType, RepoPatch, PrimOf, commute )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Named ( infopatch )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Patch.Witnesses.Ordered ( Fork(..), FL(..), (:>)(..), (+>+) )
import Darcs.UI.SelectChanges
    ( WhichChanges(First)
    , runInvertibleSelection
    , selectionConfigPrim
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions (..) )
import qualified Data.ByteString as B
import Darcs.Util.Lock ( writeDocBinFile, removeFileMayNotExist )
import Darcs.Patch.Depends ( mergeThem )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Prompt ( askUser )
import Darcs.Patch.Bundle ( parseBundle, interpretBundle, makeBundle )
import Darcs.Util.IsoDate ( getIsoDateTime )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )

unrevertDescription :: String
unrevertDescription =
 "Undo the last revert."

unrevertHelp :: Doc
unrevertHelp = text $
 "Unrevert is a rescue command in case you accidentally reverted\n" ++
 "something you wanted to keep (for example, typing `darcs rev -a`\n" ++
 "instead of `darcs rec -a`).\n" ++
 "\n" ++
 "This command may fail if the repository has changed since the revert\n" ++
 "took place.  Darcs will ask for confirmation before executing an\n" ++
 "interactive command that will DEFINITELY prevent unreversion.\n"

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = []
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext ? flags
    }

unrevert :: DarcsCommand
unrevert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "unrevert"
    , commandHelp = unrevertHelp
    , commandDescription = unrevertDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = unrevertCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc unrevertAdvancedOpts
    , commandBasicOptions = odesc unrevertBasicOpts
    , commandDefaults = defaultFlags unrevertOpts
    , commandCheckOptions = ocheck unrevertOpts
    }
  where
    unrevertBasicOpts
      = O.useIndex
      ^ O.interactive -- True
      ^ O.repoDir
      ^ O.withContext
      ^ O.diffAlgorithm
    unrevertAdvancedOpts = O.umask
    unrevertOpts = unrevertBasicOpts `withStdOpts` unrevertAdvancedOpts

unrevertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
unrevertCmd _ opts [] =
 withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $ RepoJob $ \_repository -> do
  us <- readRepo _repository
  Sealed them <- unrevertPatchBundle us
  recorded <- readRecorded _repository
  unrecorded <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -})
    O.NoLookForMoves O.NoLookForReplaces _repository Nothing
  Sealed h_them <- return $ mergeThem us them
  Sealed pw <- considerMergeToWorking _repository "unrevert"
                      YesAllowConflictsAndMark
                      NoExternalMerge NoWantGuiPause
                      (compress ? opts) (verbosity ? opts) NoReorder
                      ( UseIndex, ScanKnown, diffAlgorithm ? opts )
                      (Fork us NilFL h_them)
  let selection_config =
        selectionConfigPrim
            First "unrevert" (patchSelOpts opts)
            Nothing Nothing (Just recorded)
  (p :> skipped) <- runInvertibleSelection pw selection_config
  tentativelyAddToPending _repository p
  withSignalsBlocked $
      do _repository <- finalizeRepositoryChanges _repository YesUpdatePending (compress ? opts)
         _ <- applyToWorking _repository (verbosity ? opts) p
         debugMessage "I'm about to writeUnrevert."
         writeUnrevert _repository skipped recorded (unrecorded+>+p)
  putFinished opts "unreverting"
unrevertCmd _ _ _ = error "impossible case"

writeUnrevert :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
              => Repository rt p wR wU wT -> FL (PrimOf p) wX wY
              -> Tree IO -> FL (PrimOf p) wR wX -> IO ()
writeUnrevert _ NilFL _ _ = removeFileMayNotExist unrevertPath
writeUnrevert repository ps recorded pend =
  case commute (pend :> ps) of
    Nothing -> do really <- askUser "You will not be able to unrevert this operation! Proceed? "
                  case really of ('y':_) -> return ()
                                 _ -> exitSuccess
                  writeUnrevert repository NilFL recorded pend
    Just (p' :> _) -> do
        rep <- readRepo repository
        date <- getIsoDateTime
        info <- patchinfo date "unrevert" "anon" []
        let np = infopatch info p'
        bundle <- makeBundle (Just recorded) rep (np :>: NilFL)
        writeDocBinFile unrevertPath bundle

unrevertPatchBundle :: RepoPatch p
                    => PatchSet rt p Origin wR
                    -> IO (SealedPatchSet rt p Origin)
unrevertPatchBundle us = do
  pf <- B.readFile unrevertPath
        `catchall` fail "There's nothing to unrevert!"
  case parseBundle pf of
      Right (Sealed bundle) -> do
        case interpretBundle us bundle of
          Left msg -> fail msg
          Right ps -> return (Sealed ps)
      Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
