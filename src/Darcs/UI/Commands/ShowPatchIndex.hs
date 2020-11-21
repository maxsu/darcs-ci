module Darcs.UI.Commands.ShowPatchIndex ( showPatchIndex ) where

import Darcs.Prelude

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache, verbose )
import Darcs.UI.Options
    ( (^), oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Repository ( withRepository, RepoJob(..), repoLocation )
import Darcs.Repository.PatchIndex
    ( dumpPatchIndex, piTest, doesPatchIndexExist, isPatchIndexInSync)
import Darcs.Util.Printer ( Doc, text )

help :: Doc
help = text $
  "When given the `--verbose` flag, the command dumps the complete content\n" ++
  "of the patch index and checks its integrity."

showPatchIndex :: DarcsCommand
showPatchIndex = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "patch-index"
    , commandDescription = "Check integrity of patch index"
    , commandHelp = help
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = showPatchIndexCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showPatchIndexBasicOpts
    , commandDefaults = defaultFlags showPatchIndexOpts
    , commandCheckOptions = ocheck showPatchIndexOpts
    }
  where
    showPatchIndexBasicOpts = O.nullFlag ^ O.repoDir
    showPatchIndexOpts = showPatchIndexBasicOpts `withStdOpts` oid

showPatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPatchIndexCmd _ opts _
  | verbose opts =
    withRepository (useCache ? opts) $ RepoJob $ \repo ->
      let loc = repoLocation repo in dumpPatchIndex loc >> piTest loc
  | otherwise =
    withRepository (useCache ? opts) $ RepoJob $ \repo -> do
    ex <- doesPatchIndexExist (repoLocation repo)
    if ex then do
          sy <- isPatchIndexInSync repo
          if sy
            then putStrLn "Patch Index is in sync with repo."
            else putStrLn "Patch Index is outdated. Run darcs optimize enable-patch-index"
     else putStrLn "Patch Index is not yet created. Run darcs optimize enable-patch-index"

