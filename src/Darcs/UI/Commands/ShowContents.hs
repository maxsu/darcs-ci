--  Copyright (C) 2007 Eric Kow
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

module Darcs.UI.Commands.ShowContents ( showContents ) where

import Control.Monad ( filterM, forM_, forM, when )
import System.IO ( stdout )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Darcs.Prelude

import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, findRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, useCache, pathsFromArgs )
import Darcs.UI.Options ( (^), oid, odesc, ocheck, defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Patch.Match ( patchSetMatch )
import Darcs.Repository ( withRepository, RepoJob(..), readRecorded )
import Darcs.Util.Lock ( withDelayedDir )
import Darcs.Repository.Match ( getRecordedUpToMatch )
import Darcs.Util.Tree.Plain( readPlainTree )
import qualified Darcs.Util.Tree.Monad as TM
import Darcs.Util.Path( AbsolutePath )
import Darcs.Util.Printer ( Doc, text )

showContentsDescription :: String
showContentsDescription = "Outputs a specific version of a file."

showContentsHelp :: Doc
showContentsHelp = text $
  "Show contents can be used to display an earlier version of some file(s).\n"++
  "If you give show contents no version arguments, it displays the recorded\n"++
  "version of the file(s).\n"

showContents :: DarcsCommand
showContents = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "contents"
    , commandHelp = showContentsHelp
    , commandDescription = showContentsDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[FILE]..."]
    , commandCommand = showContentsCmd
    , commandPrereq = findRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showContentsBasicOpts
    , commandDefaults = defaultFlags showContentsOpts
    , commandCheckOptions = ocheck showContentsOpts
    }
  where
    showContentsBasicOpts = O.matchUpToOne ^ O.repoDir
    showContentsOpts = O.matchUpToOne ^ O.repoDir `withStdOpts` oid

showContentsCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showContentsCmd _ _ [] = fail "show contents needs at least one argument."
showContentsCmd fps opts args = do
  paths <- pathsFromArgs fps args
  when (null paths) $ fail "No valid repository paths were given."
  let matchFlags = parseFlags O.matchUpToOne opts
  withRepository (useCache ? opts) $ RepoJob $ \repository -> do
    let readContents = do
          okpaths <- filterM TM.fileExists paths
          forM okpaths $ \f -> (B.concat . BL.toChunks) `fmap` TM.readFile f
        -- Note: The two calls to execReadContents below are from
        -- different working directories. This matters despite our
        -- use of virtualTreeIO.
        execReadContents tree = fst `fmap` TM.virtualTreeIO readContents tree
    files <-
      case patchSetMatch matchFlags of
        Just psm ->
               withDelayedDir "show.contents" $ \_ -> do
                 -- this call populates our temporary directory, but note that
                 -- it does so lazily: the tree gets (partly) expanded inside
                 -- execReadContents, so it is important that we execute the
                 -- latter from the same working directory.
                 getRecordedUpToMatch repository psm
                 readPlainTree "." >>= execReadContents
        Nothing ->
               -- we can use the existing pristine tree because we don't modify
               -- anything in this case
               readRecorded repository >>= execReadContents
    forM_ files $ B.hPut stdout
