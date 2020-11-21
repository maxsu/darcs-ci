--  Copyright (C) 2002-2005 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Revert ( revert ) where

import Darcs.Prelude

import Control.Monad ( void )

import Darcs.UI.Flags
    ( DarcsFlag
    , diffAlgorithm
    , diffingOpts
    , dryRun
    , isInteractive
    , pathSetFromArgs
    , umask
    , useCache
    , verbosity
    , withContext
    )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amInHashedRepository
    , nodefaults
    , putInfo
    , putFinished
    , withStdOpts
    )
import Darcs.UI.Commands.Util ( announceFiles )
import Darcs.UI.Commands.Unrevert ( writeUnrevert )
import Darcs.UI.Completion ( modifiedFileArgs )

import Darcs.Util.Global ( debugMessage )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , applyToWorking
    , readRecorded
    , unrecordedChanges
    )
import Darcs.Patch ( invert, effectOnPaths, commuteFL )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..)
    , (:>)(..)
    , nullFL
    , (+>>+)
    , reverseFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.UI.SelectChanges
    ( WhichChanges(Last)
    , selectionConfigPrim
    , runInvertibleSelection
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Patch.TouchesFiles ( chooseTouching )


revertDescription :: String
revertDescription = "Discard unrecorded changes."

revertHelp :: Doc
revertHelp = text $
 "The `darcs revert` command discards unrecorded changes the working\n" ++
 "tree.  As with `darcs record`, you will be asked which hunks (changes)\n" ++
 "to revert.  The `--all` switch can be used to avoid such prompting. If\n" ++
 "files or directories are specified, other parts of the working tree\n" ++
 "are not reverted.\n" ++
 "\n" ++
 "In you accidentally reverted something you wanted to keep (for\n" ++
 "example, typing `darcs rev -a` instead of `darcs rec -a`), you can\n" ++
 "immediately run `darcs unrevert` to restore it.  This is only\n" ++
 "guaranteed to work if the repository has not changed since `darcs\n" ++
 "revert` ran.\n"

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = []
    , S.interactive = isInteractive True flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext ? flags
    }

revert :: DarcsCommand
revert = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "revert"
    , commandHelp = revertHelp
    , commandDescription = revertDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = revertCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = modifiedFileArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc revertAdvancedOpts
    , commandBasicOptions = odesc revertBasicOpts
    , commandDefaults = defaultFlags revertOpts
    , commandCheckOptions = ocheck revertOpts
    }
  where
    revertBasicOpts
      = O.interactive -- True
      ^ O.repoDir
      ^ O.withContext
      ^ O.diffAlgorithm
    revertAdvancedOpts = O.useIndex ^ O.umask
    revertOpts = revertBasicOpts `withStdOpts` revertAdvancedOpts

revertCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
revertCmd fps opts args =
 withRepoLock (dryRun ? opts) (useCache ? opts) YesUpdatePending (umask ? opts) $
 RepoJob $ \repository -> do
  files <- pathSetFromArgs fps args
  announceFiles (verbosity ? opts) files "Reverting changes in"
  changes <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -})
    O.NoLookForMoves O.NoLookForReplaces repository files
  let pre_changed_files = effectOnPaths (invert changes) <$> files
  recorded <- readRecorded repository
  Sealed touching_changes <- return (chooseTouching pre_changed_files changes)
  case touching_changes of
    NilFL -> putInfo opts "There are no changes to revert!"
    _ -> do
      let selection_config = selectionConfigPrim
                                Last "revert" (patchSelOpts opts)
                                (Just (reversePrimSplitter (diffAlgorithm ? opts)))
                                pre_changed_files (Just recorded)
      norevert :> torevert <- runInvertibleSelection changes selection_config
      if nullFL torevert
       then putInfo opts $
              "If you don't want to revert after all, that's fine with me!"
       else withSignalsBlocked $ do
                 addToPending repository (O.useIndex ? opts) $ invert torevert
                 debugMessage "About to write the unrevert file."
                 {- The user has split unrecorded into the sequence 'norevert' then 'torevert',
                    which is natural as the bit we keep in unrecorded should have recorded
                    as the context.

                    But the unrevert patch also needs to have recorded as the context, not
                    unrecorded (which can be changed by the user at any time).

                    So we need to commute 'torevert' with 'norevert', and if that fails then
                    we need to keep some of 'norevert' in the actual unrevert patch so it
                    still makes sense. The use of genCommuteWhatWeCanRL minimises the amount
                    of 'norevert' that we need to keep.
                 -}
                 case genCommuteWhatWeCanRL commuteFL (reverseFL norevert :> torevert) of
                   deps :> torevert' :> _ ->
                      writeUnrevert repository (deps +>>+ torevert') recorded NilFL
                 debugMessage "About to apply to the working tree."
                 void $ applyToWorking repository (verbosity ? opts) (invert torevert)
      putFinished opts "reverting"
