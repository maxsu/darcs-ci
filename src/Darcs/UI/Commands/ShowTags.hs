--  Copyright (C) 2007 Florian Weimer
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

module Darcs.UI.Commands.ShowTags
    ( showTags
    ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Data.Maybe ( fromMaybe )
import System.IO ( stderr, hPutStrLn )

import Darcs.Patch.Set ( PatchSet, patchSetTags )
import Darcs.Repository ( readRepo, withRepositoryLocation, RepoJob(..) )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache, getRepourl )
import Darcs.UI.Options ( oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer ( Doc, formatText )

showTagsDescription :: String
showTagsDescription = "Show all tags in the repository."

showTagsHelp :: Doc
showTagsHelp = formatText 80
    [ "The tags command writes a list of all tags in the repository to "
      ++ "standard output."
    , "Tab characters (ASCII character 9) in tag names are changed to spaces "
      ++ "for better interoperability with shell tools. A warning is printed "
      ++ "if this happens."
    ]

showTags :: DarcsCommand
showTags = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "tags"
    , commandHelp = showTagsHelp
    , commandDescription = showTagsDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = tagsCmd
    , commandPrereq = findRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showTagsBasicOpts
    , commandDefaults = defaultFlags showTagsOpts
    , commandCheckOptions = ocheck showTagsOpts
    }
  where
    showTagsBasicOpts = O.possiblyRemoteRepo
    showTagsOpts = showTagsBasicOpts `withStdOpts` oid

tagsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
tagsCmd _ opts _ = let repodir = fromMaybe "." (getRepourl opts) in
    withRepositoryLocation (useCache ? opts) repodir $ RepoJob $ \repo ->
        readRepo repo >>= printTags
  where
    printTags :: PatchSet rt p wW wZ -> IO ()
    printTags = mapM_ process . patchSetTags
    process :: String -> IO ()
    process t = normalize t t False >>= putStrLn
    normalize :: String -> String -> Bool -> IO String
    normalize _ [] _ = return []
    normalize t (x : xs) flag =
        if x == '\t' then do
            unless flag $
                hPutStrLn stderr $ "warning: tag with TAB character: " ++ t
            rest <- normalize t xs True
            return $ ' ' : rest
        else do
            rest <- normalize t xs flag
            return $ x : rest
