--  Copyright (C) 2007 Kevin Quick
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

module Darcs.UI.Commands.ShowRepo ( showRepo ) where

import Darcs.Prelude

import Data.Char ( toLower, isSpace )
import Data.List ( intercalate )
import Control.Monad ( when, unless, liftM )
import Text.Html ( tag, stringToHtml )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.UI.Flags ( DarcsFlag, useCache, hasXmlOutput, verbose, enumeratePatches )
import Darcs.UI.Options ( (^), oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.Repository
    ( Repository
    , repoFormat
    , repoLocation
    , repoPristineType
    , repoCache
    , withRepository
    , RepoJob(..)
    , readRepo )
import Darcs.Repository.Hashed( repoXor )
import Darcs.Repository.PatchIndex ( isPatchIndexDisabled, doesPatchIndexExist )
import Darcs.Repository.Prefs ( getPreflist, getMotd )
import Darcs.Patch ( IsRepoType, RepoPatch )
import Darcs.Patch.Set ( patchSet2RL )
import Darcs.Patch.Witnesses.Ordered ( lengthRL )
import qualified Data.ByteString.Char8 as BC  (unpack)
import Darcs.Patch.Apply( ApplyState )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.Tree ( Tree )

showRepoHelp :: Doc
showRepoHelp = text $
 "The `darcs show repo` command displays statistics about the current\n" ++
 "repository, allowing third-party scripts to access this information\n" ++
 "without inspecting `_darcs` directly (and without breaking when the\n" ++
 "`_darcs` format changes).\n" ++
 "\n" ++
 "The 'Weak Hash' identifies the set of patches of a repository independently\n" ++
 "of ordering. It can be used to easily compare two repositories of a same\n" ++
 "project. It is not cryptographically secure.\n" ++
 "\n" ++
 "By default, output includes statistics that require walking through the patches\n" ++
 "recorded in the repository, namely the 'Weak Hash' and the count of patches.\n" ++
 "If this data isn't needed, use `--no-enum-patches` to accelerate this command\n" ++
 "from O(n) to O(1).\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The `--xml-output`\n" ++
 "option can be used to generate output for machine postprocessing.\n"

showRepoDescription :: String
showRepoDescription = "Show repository summary information"

showRepo :: DarcsCommand
showRepo = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "repo"
    , commandHelp = showRepoHelp
    , commandDescription = showRepoDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = repoCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showRepoBasicOpts
    , commandDefaults = defaultFlags showRepoOpts
    , commandCheckOptions = ocheck showRepoOpts
    }
  where
    showRepoBasicOpts = O.repoDir ^ O.xmlOutput ^ O.enumPatches
    showRepoOpts = showRepoBasicOpts `withStdOpts` oid

repoCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
repoCmd _ opts _ =
  let put_mode = if hasXmlOutput opts then showInfoXML else showInfoUsr
  in withRepository (useCache ? opts) $
     RepoJob $ \repository ->
       actuallyShowRepo (putInfo put_mode) repository opts

-- Some convenience functions to output a labelled text string or an
-- XML tag + value (same API).  If no value, output is suppressed
-- entirely.  Borrow some help from Text.Html to perform XML output.

type ShowInfo = String -> String -> String

showInfoXML :: ShowInfo
showInfoXML t i = show $ tag (safeTag t) $ stringToHtml i

safeTag :: String -> String
safeTag [] = []
safeTag (' ':cs) = safeTag cs
safeTag ('#':cs) = "num_" ++ safeTag cs
safeTag (c:cs) = toLower c : safeTag cs

-- labelled strings: labels are right-aligned at 15 characters;
-- subsequent lines in multi-line output are indented accordingly.
showInfoUsr :: ShowInfo
showInfoUsr t i = replicate (15 - length t) ' ' ++ t ++ ": " ++
                  intercalate ('\n' : replicate 17 ' ') (lines i) ++ "\n"

type PutInfo = String -> String -> IO ()
putInfo :: ShowInfo -> PutInfo
putInfo m t i = unless (null i) (putStr $ m t i)

-- Primary show-repo operation.  Determines ordering of output for
-- sub-displays.  The `out' argument is one of the above operations to
-- output a labelled text string or an XML tag and contained value.

actuallyShowRepo
  :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => PutInfo -> Repository rt p wR wU wR -> [DarcsFlag] -> IO ()
actuallyShowRepo out r opts = do
  when (hasXmlOutput opts) (putStr "<repository>\n")
  when (verbose opts) (out "Show" $ show r)
  out "Format" $ showInOneLine $ repoFormat r
  let loc = repoLocation r
  out "Root" loc
  out "PristineType" $ show $ repoPristineType r
  out "Cache" $ showInOneLine $ repoCache r
  piExists <- doesPatchIndexExist loc
  piDisabled <- isPatchIndexDisabled loc
  out "PatchIndex" $
    case (piExists, piDisabled) of
      (_, True) -> "disabled"
      (True, False) -> "enabled"
      (False, False) -> "enabled, but not yet created"
  showRepoPrefs out
  when (enumeratePatches opts) (do numPatches r >>= (out "Num Patches" . show)
                                   showXor out r)
  showRepoMOTD out r
  when (hasXmlOutput opts) (putStr "</repository>\n")

showXor :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
        => PutInfo -> Repository rt p wR wU wR -> IO ()
showXor out repo = do
  theXor <- repoXor repo
  out "Weak Hash" (show theXor)

-- Most of the actual elements being displayed are part of the Show
-- class; that's fine for a Haskeller, but not for the common user, so
-- the routines below work to provide more human-readable information
-- regarding the repository elements.

showInOneLine :: Show a => a -> String
showInOneLine = intercalate ", " . lines . show

showRepoPrefs :: PutInfo -> IO ()
showRepoPrefs out = do
    getPreflist "prefs" >>= mapM_ prefOut
    getPreflist "author" >>= out "Author" . unlines
    getPreflist "defaultrepo" >>= out "Default Remote" . unlines
  where prefOut = uncurry out . (\(p,v) -> (p++" Pref", dropWhile isSpace v)) . break isSpace

showRepoMOTD :: PutInfo -> Repository rt p wR wU wR -> IO ()
showRepoMOTD out repo = getMotd (repoLocation repo) >>= out "MOTD" . BC.unpack

-- Support routines to provide information used by the PutInfo operations above.

numPatches :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree) => Repository rt p wR wU wR -> IO Int
numPatches r = (lengthRL . patchSet2RL) `liftM` readRepo r
