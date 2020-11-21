-- Copyright (C) 2009 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.UI.Commands.ShowIndex
    ( showIndex
    , showPristine
    ) where

import Darcs.Prelude

import Control.Monad ( (>=>) )
import Darcs.UI.Flags ( DarcsFlag, useCache )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInRepository )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Options ( (^), oid, odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository ( withRepository, RepoJob(..), readIndex )
import Darcs.Repository.State ( readRecorded )

import Darcs.Util.Hash( encodeBase16, Hash( NoHash ) )
import Darcs.Util.Tree( list, expand, itemHash, Tree, TreeItem( SubTree ) )
import Darcs.Util.Index( updateIndex, listFileIDs )
import Darcs.Util.Path( anchorPath, AbsolutePath, floatPath )
import Darcs.Util.Printer ( Doc, text )

import System.Posix.Types ( FileID )

import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( fromJust )
import qualified Data.Map as M ( Map, lookup, fromList )

showIndexHelp :: Doc
showIndexHelp = text $
  "The `darcs show index` command lists all version-controlled files and " ++
  "directories along with their hashes as stored in `_darcs/index`. " ++
  "For files, the fields correspond to file size, sha256 of the current " ++
  "file content and the filename."

showIndex :: DarcsCommand
showIndex = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "index"
    , commandDescription = "Dump contents of working tree index."
    , commandHelp = showIndexHelp
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = showIndexCmd
    , commandPrereq = amInRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc showIndexBasicOpts
    , commandDefaults = defaultFlags showIndexOpts
    , commandCheckOptions = ocheck showIndexOpts
    }
  where
    showIndexBasicOpts = O.nullFlag ^ O.repoDir
    showIndexOpts = showIndexBasicOpts `withStdOpts` oid

dump :: [DarcsFlag] -> Maybe (M.Map FilePath FileID) -> Tree IO -> IO ()
dump opts fileids tree = do
  let line | O.nullFlag ? opts = \t -> putStr t >> putChar '\0'
           | otherwise = putStrLn
      output (p, i) = do
        let hash = case itemHash i of
                     NoHash -> "(no hash available)"
                     h -> BC.unpack $ encodeBase16 h
            path = anchorPath "" p
            isdir = case i of
                      SubTree _ -> "/"
                      _ -> ""
            fileid = case fileids of
                       Nothing -> ""
                       Just fileids' -> " " ++ (show $ fromJust $ M.lookup path fileids')
        line $ hash ++ fileid ++ " " ++ path ++ isdir
  x <- expand tree
  mapM_ output $ (floatPath ".", SubTree x) : list x

showIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showIndexCmd _ opts _ = withRepository (useCache ? opts) $ RepoJob $ \repo ->
  do index <- readIndex repo
     index_tree <- updateIndex index
     fileids <- (M.fromList . map (\((a,_),b) -> (anchorPath "" a,b))) <$> listFileIDs index
     dump opts (Just fileids) index_tree

showPristineCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
showPristineCmd _ opts _ = withRepository (useCache ? opts) $ RepoJob $
  readRecorded >=> dump opts Nothing

showPristineHelp :: Doc
showPristineHelp = text $
  "The `darcs show pristine` command lists all version-controlled files " ++
  "and directories along with the hashes of their pristine copies. " ++
  "For files, the fields correspond to file size, sha256 of the pristine " ++
  "file content and the filename."

showPristine :: DarcsCommand
showPristine = showIndex
    { commandName = "pristine"
    , commandDescription = "Dump contents of pristine cache."
    , commandHelp = showPristineHelp
    , commandCommand = showPristineCmd
    }
