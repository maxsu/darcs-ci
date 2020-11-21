--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.UI.Commands.Record
    ( record
    , commit
    ) where

import Darcs.Prelude
import Data.Foldable ( traverse_ )

import Control.Exception ( handleJust )
import Control.Monad ( when, unless, void )
import Data.Char ( ord )
import System.Exit ( exitFailure, exitSuccess, ExitCode(..) )
import System.Directory ( removeFile )

import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Repository
    ( Repository
    , withRepoLock
    , RepoJob(..)
    , tentativelyAddPatch
    , finalizeRepositoryChanges
    , invalidateIndex
    , readPendingAndWorking
    , readRecorded
    )
import Darcs.Repository.Pending ( tentativelyRemoveFromPW )

import Darcs.Patch ( IsRepoType, RepoPatch, PrimOf, sortCoalesceFL )
import Darcs.Patch.Named ( infopatch, adddeps )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:>)(..), nullFL, (+>+) )
import Darcs.Patch.Info ( PatchInfo, patchinfo )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Split ( primSplitter )
import Darcs.UI.SelectChanges
    (  WhichChanges(..)
    , selectionConfigPrim
    , runInvertibleSelection
    , askAboutDepends
    )
import qualified Darcs.UI.SelectChanges as S ( PatchSelectionOptions(..) )
import Darcs.Util.Path ( AnchoredPath, displayPath, AbsolutePath )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , nodefaults
    , commandAlias
    , setEnvDarcsFiles
    , setEnvDarcsPatches
    , amInHashedRepository
    )
import Darcs.UI.Commands.Util ( announceFiles, filterExistingPaths,
                                testTentativeAndMaybeExit )
import Darcs.UI.Completion ( modifiedFileArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , fileHelpAuthor
    , getAuthor
    , getDate
    , diffOpts
    , scanKnown
    , pathSetFromArgs
    )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck, oparse, defaultFlags )
import Darcs.UI.PatchHeader ( getLog )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdatePending (..), DryRun(NoDryRun), ScanKnown(..) )
import Darcs.Util.Exception ( clarifyErrors )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Progress ( debugMessage )
import Darcs.Util.Global ( darcsLastMessage )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Util.Printer
    ( Doc
    , ($+$)
    , (<+>)
    , formatWords
    , pathlist
    , putDocLn
    , text
    , vcat
    , vsep
    )
import Darcs.Util.Tree( Tree )

recordHelp :: Doc
recordHelp =
  vsep (map formatWords
  [ [ "The `darcs record` command is used to create a patch from changes in"
    , "the working tree.  If you specify a set of files and directories,"
    , "changes to other files will be skipped."
    ]
  , [ "Every patch has a name, an optional description, an author and a date."
    ]
  , [ "Darcs will launch a text editor (see `darcs help environment`) after the"
    , "interactive selection, to let you enter the patch name (first line) and"
    , "the patch description (subsequent lines)."
    ]
  , [ "You can supply the patch name in advance with the `-m` option, in which"
    , "case no text editor is launched, unless you use `--edit-long-comment`."
    ]
  , [ "The patch description is an optional block of free-form text.  It is"
    , "used to supply additional information that doesn't fit in the patch"
    , "name.  For example, it might include a rationale of WHY the change was"
    , "necessary."
    ]
  , [ "A technical difference between patch name and patch description, is"
    , "that matching with the flag `-p` is only done on patch names."
    ]
  , [ "Finally, the `--logfile` option allows you to supply a file that already"
    , "contains the patch name and description.  This is useful if a previous"
    , "record failed and left a `_darcs/patch_description.txt` file."
    ]
  , fileHelpAuthor
  , [ "If you want to manually define any explicit dependencies for your patch,"
    , "you can use the `--ask-deps` flag. Some dependencies may be automatically"
    , "inferred from the patch's content and cannot be removed. A patch with"
    , "specific dependencies can be empty."
    ]
  , [ "The patch date is generated automatically.  It can only be spoofed by"
    , "using the `--pipe` option."
    ]
  , [ "If you run record with the `--pipe` option, you will be prompted for"
    , "the patch date, author, and the long comment. The long comment will extend"
    , "until the end of file or stdin is reached. This interface is intended for"
    , "scripting darcs, in particular for writing repository conversion scripts."
    , "The prompts are intended mostly as a useful guide (since scripts won't"
    , "need them), to help you understand the input format. Here's an example of"
    , "what the `--pipe` prompts look like:"
    ]
  ])
  $+$ vcat
    [ "    What is the date? Mon Nov 15 13:38:01 EST 2004"
    , "    Who is the author? David Roundy"
    , "    What is the log? One or more comment lines"
    ]
  $+$ vsep (map formatWords
  [ [ "If a test command has been defined with `darcs setpref`, attempting to"
    , "record a patch will cause the test command to be run in a clean copy"
    , "of the working tree (that is, including only recorded changes).  If"
    , "the test fails, you will be offered to abort the record operation."
    ]
  , [ "The `--set-scripts-executable` option causes scripts to be made"
    , "executable in the clean copy of the working tree, prior to running the"
    , "test.  See `darcs clone` for an explanation of the script heuristic."
    ]
  , [ "If your test command is tediously slow (e.g. `make all`) and you are"
    , "recording several patches in a row, you may wish to use `--no-test` to"
    , "skip all but the final test."
    ]
  , [ "To see some context (unchanged lines) around each change, use the"
    , "`--unified` option."
    ]
  ])

recordBasicOpts :: DarcsOption a
                   (Maybe String
                    -> Maybe String
                    -> O.TestChanges
                    -> Maybe Bool
                    -> Bool
                    -> Bool
                    -> Maybe O.AskLongComment
                    -> O.LookFor
                    -> Maybe String
                    -> O.WithContext
                    -> O.DiffAlgorithm
                    -> a)
recordBasicOpts
    = O.patchname
    ^ O.author
    ^ O.testChanges
    ^ O.interactive
    ^ O.pipe
    ^ O.askDeps
    ^ O.askLongComment
    ^ O.lookfor
    ^ O.repoDir
    ^ O.withContext
    ^ O.diffAlgorithm

recordAdvancedOpts :: DarcsOption a
                      (O.Logfile -> O.Compression -> O.UseIndex -> O.UMask -> O.SetScriptsExecutable -> O.IncludeBoring -> a)
recordAdvancedOpts = O.logfile ^ O.compress ^ O.useIndex ^ O.umask ^ O.setScriptsExecutable ^ O.includeBoring

data RecordConfig = RecordConfig
    { patchname :: Maybe String
    , author :: Maybe String
    , testChanges :: O.TestChanges
    , interactive :: Maybe Bool
    , pipe :: Bool
    , askDeps :: Bool
    , askLongComment :: Maybe O.AskLongComment
    , lookfor :: O.LookFor
    , _workingRepoDir :: Maybe String
    , withContext :: O.WithContext
    , diffAlgorithm :: O.DiffAlgorithm
    , verbosity :: O.Verbosity
    , logfile :: O.Logfile
    , compress :: O.Compression
    , useIndex :: O.UseIndex
    , umask :: O.UMask
    , sse :: O.SetScriptsExecutable
    , includeBoring :: O.IncludeBoring
    , useCache :: O.UseCache
    }

recordConfig :: [DarcsFlag] -> RecordConfig
recordConfig = oparse (recordBasicOpts ^ O.verbosity ^ recordAdvancedOpts ^ O.useCache) RecordConfig

record :: DarcsCommand
record = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "record"
    , commandHelp = recordHelp
    , commandDescription = "Create a patch from unrecorded changes."
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE or DIRECTORY]..."]
    , commandCommand = recordCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = modifiedFileArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc recordAdvancedOpts
    , commandBasicOptions = odesc recordBasicOpts
    , commandDefaults = defaultFlags recordOpts
    , commandCheckOptions = ocheck recordOpts
    }
  where
    recordOpts = recordBasicOpts `withStdOpts` recordAdvancedOpts

-- | commit is an alias for record
commit :: DarcsCommand
commit = commandAlias "commit" Nothing record

reportNonExisting :: ScanKnown -> ([AnchoredPath], [AnchoredPath]) -> IO ()
reportNonExisting scan (paths_only_in_working, _) = do
  unless (scan /= ScanKnown || null paths_only_in_working) $  putDocLn $
    "These paths are not yet in the repository and will be added:" <+>
    pathlist (map displayPath paths_only_in_working)

recordCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
recordCmd fps flags args = do
    let cfg = recordConfig flags
    checkNameIsNotOption (patchname cfg) (isInteractive cfg)
    withRepoLock NoDryRun (useCache cfg) YesUpdatePending (umask cfg) $ RepoJob $ \(repository :: Repository rt p wR wU wR) -> do
      let scan = scanKnown (O.adds (lookfor cfg)) (includeBoring cfg)
      existing_files <- do
        files <- pathSetFromArgs fps args
        files' <-
          traverse
            (filterExistingPaths
              repository (verbosity cfg) (useIndex cfg) scan (O.moves (lookfor cfg)))
            files
        when (verbosity cfg /= O.Quiet) $
            traverse_ (reportNonExisting scan) files'
        let files'' = fmap snd files'
        when (files'' == Just []) $
            fail "None of the files you specified exist."
        return files''
      announceFiles (verbosity cfg) existing_files "Recording changes in"
      debugMessage "About to get the unrecorded changes."
      changes <- readPendingAndWorking (diffingOpts cfg)
                   (O.moves (lookfor cfg)) (O.replaces (lookfor cfg))
                   repository existing_files
      debugMessage "I've got unrecorded changes."
      case changes of
          NilFL :> NilFL | not (askDeps cfg) -> do
              -- We need to grab any input waiting for us, since we
              -- might break scripts expecting to send it to us; we
              -- don't care what that input is, though.
              void (getDate (pipe cfg))
              putStrLn "No changes!"
              exitFailure
          _ -> doRecord repository cfg existing_files changes

-- | Check user specified patch name is not accidentally a command line flag
checkNameIsNotOption :: Maybe String -> Bool -> IO ()
checkNameIsNotOption Nothing     _      = return ()
checkNameIsNotOption _           False  = return ()
checkNameIsNotOption (Just name) True   =
    when (length name == 1 || (length name == 2 && head name == '-')) $ do
        confirmed <- promptYorn $ "You specified " ++ show name ++ " as the patch name. Is that really what you want?"
        unless confirmed $ putStrLn "Okay, aborting the record." >> exitFailure

doRecord :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
         => Repository rt p wR wU wR -> RecordConfig -> Maybe [AnchoredPath]
         -> (FL (PrimOf p) :> FL (PrimOf p)) wR wU -> IO ()
doRecord repository cfg files pw@(pending :> working) = do
    date <- getDate (pipe cfg)
    my_author <- getAuthor (author cfg) (pipe cfg)
    debugMessage "I'm slurping the repository."
    pristine <- readRecorded repository
    debugMessage "About to select changes..."
    (chs :> _ ) <- runInvertibleSelection (sortCoalesceFL $ pending +>+ working) $
                      selectionConfigPrim
                          First "record" (patchSelOpts cfg)
                          (Just (primSplitter (diffAlgorithm cfg)))
                          files (Just pristine)
    when (not (askDeps cfg) && nullFL chs) $
              do putStrLn "Ok, if you don't want to record anything, that's fine!"
                 exitSuccess
    handleJust onlySuccessfulExits (\_ -> return ()) $
             do deps <- if askDeps cfg
                        then askAboutDepends repository chs (patchSelOpts cfg) []
                        else return []
                when (askDeps cfg) $ debugMessage "I've asked about dependencies."
                if nullFL chs && null deps
                  then putStrLn "Ok, if you don't want to record anything, that's fine!"
                  else do setEnvDarcsFiles chs
                          (name, my_log, logf) <- getLog (patchname cfg) (pipe cfg) (logfile cfg) (askLongComment cfg) Nothing chs
                          debugMessage ("Patch name as received from getLog: " ++ show (map ord name))
                          doActualRecord repository cfg name date my_author my_log logf deps chs pw

doActualRecord :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
               => Repository rt p wR wU wR
               -> RecordConfig
               -> String -> String -> String
               -> [String] -> Maybe String
               -> [PatchInfo] -> FL (PrimOf p) wR wX
               -> (FL (PrimOf p) :> FL (PrimOf p)) wR wU -> IO ()
doActualRecord _repository cfg name date my_author my_log logf deps chs
      (pending :> working) = do
    debugMessage "Writing the patch file..."
    myinfo <- patchinfo date name my_author my_log
    let mypatch = infopatch myinfo $ progressFL "Writing changes:" chs
    let pia = n2pia $ adddeps mypatch deps
    _repository <-
      tentativelyAddPatch _repository (compress cfg) (verbosity cfg)
        NoUpdatePending pia
    invalidateIndex _repository
    debugMessage "Applying to pristine..."
    testTentativeAndMaybeExit _repository (verbosity cfg) (testChanges cfg)
      (sse cfg) (isInteractive cfg) ("you have a bad patch: '" ++ name ++ "'")
      "record it" (Just failuremessage)
    tentativelyRemoveFromPW _repository chs pending working
    _repository <-
      finalizeRepositoryChanges _repository YesUpdatePending (compress cfg)
      `clarifyErrors` failuremessage
    debugMessage "Syncing timestamps..."
    removeLogFile logf
    unless (verbosity cfg == O.Quiet) $
      putDocLn $ text $ "Finished recording patch '" ++ name ++ "'"
    setEnvDarcsPatches (pia :>: NilFL)
  where
    removeLogFile :: Maybe String -> IO ()
    removeLogFile Nothing = return ()
    removeLogFile (Just lf)
      | lf == darcsLastMessage = return ()
      | otherwise = removeFile lf
    failuremessage =
      "Failed to record patch '" ++ name ++ "'" ++
        case logf of
          Just lf -> "\nLogfile left in " ++ lf ++ "."
          Nothing -> ""

onlySuccessfulExits :: ExitCode -> Maybe ()
onlySuccessfulExits ExitSuccess = Just ()
onlySuccessfulExits _ = Nothing

patchSelOpts :: RecordConfig -> S.PatchSelectionOptions
patchSelOpts cfg = S.PatchSelectionOptions
    { S.verbosity = verbosity cfg
    , S.matchFlags = []
    , S.interactive = isInteractive cfg
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    , S.withContext = withContext cfg
    }

diffingOpts :: RecordConfig -> (O.UseIndex, O.ScanKnown, O.DiffAlgorithm)
diffingOpts cfg = diffOpts (useIndex cfg) (O.adds (lookfor cfg)) O.NoIncludeBoring (diffAlgorithm cfg)

isInteractive :: RecordConfig -> Bool
isInteractive = maybe True id . interactive
