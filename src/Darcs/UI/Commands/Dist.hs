--  Copyright (C) 2003 David Roundy
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
-- Module      : Darcs.UI.Commands.Dist
-- Copyright   : 2003 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.UI.Commands.Dist
    (
      dist
    , doFastZip -- libdarcs export
    , doFastZip'
    ) where

import Darcs.Prelude hiding ( writeFile )

import Data.ByteString.Lazy ( writeFile )
import Control.Monad ( when )
import System.Directory ( createDirectory, setCurrentDirectory )
import System.Process ( system )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath.Posix ( takeFileName, (</>) )

import Darcs.Util.Workaround ( getCurrentDirectory )
import Codec.Archive.Tar ( pack, write )
import Codec.Archive.Tar.Entry ( entryPath )
import Codec.Compression.GZip ( compress )

import Codec.Archive.Zip ( emptyArchive, fromArchive, addEntryToArchive, toEntry )
import Darcs.Util.External ( fetchFilePS, Cachable( Uncachable ) )
import Darcs.Repository.Inventory ( peekPristineHash )
import Darcs.Repository.HashedIO ( pathsAndContents )
import Darcs.Repository.Paths ( hashedInventoryPath )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Darcs.UI.Flags as F ( DarcsFlag, useCache )
import qualified Darcs.UI.Flags as F ( setScriptsExecutable )
import Darcs.UI.Options
    ( (^), oid, odesc, ocheck
    , defaultFlags, parseFlags, (?)
    )
import qualified Darcs.UI.Options.All as O

import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository
    , putVerbose, putInfo
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.Util.Lock ( withTempDir )
import Darcs.Patch.Match ( patchSetMatch )
import Darcs.Repository.Match ( getRecordedUpToMatch )
import Darcs.Repository ( withRepository, withRepositoryLocation, RepoJob(..),
                          setScriptsExecutable, repoCache,
                          createPartialsPristineDirectoryTree )
import Darcs.Repository.Prefs ( getPrefval )

import Darcs.Util.DateTime ( getCurrentTime, toSeconds )
import Darcs.Util.Path ( AbsolutePath, toFilePath, anchoredRoot )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Printer ( Doc, text, vcat )


distDescription :: String
distDescription = "Create a distribution archive."

distHelp :: Doc
distHelp = text $ unlines
  [ "`darcs dist` creates a compressed archive in the repository's root"
  , "directory, containing the recorded state of the working tree"
  , "(unrecorded changes and the `_darcs` directory are excluded)."
  , "The command accepts matchers to create an archive of some past"
  , "repository state, for instance `--tag`."
  , ""
  , "By default, the archive (and the top-level directory within the"
  , "archive) has the same name as the repository, but this can be"
  , "overridden with the `--dist-name` option."
  , ""
  , "If a predist command is set (see `darcs setpref`), that command will"
  , "be run on the recorded state prior to archiving.  For example,"
  , "autotools projects would set it to `autoconf && automake`."
  , ""
  , "If `--zip` is used, matchers and the predist command are ignored."
  ]

dist :: DarcsCommand
dist = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "dist"
    , commandHelp = distHelp
    , commandDescription = distDescription
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandCommand = distCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = odesc distBasicOpts
    , commandDefaults = defaultFlags distOpts
    , commandCheckOptions = ocheck distOpts
    }
  where
    distBasicOpts
      = O.distname
      ^ O.distzip
      ^ O.repoDir
      ^ O.matchUpToOne
      ^ O.setScriptsExecutable
      ^ O.storeInMemory
    distOpts = distBasicOpts `withStdOpts` oid

distCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
distCmd _ opts _ | O.distzip ? opts = doFastZip opts
distCmd _ opts _ = withRepository (useCache ? opts) $ RepoJob $ \repository -> do
  let matchFlags = parseFlags O.matchUpToOne opts
  formerdir <- getCurrentDirectory
  let distname = getDistName formerdir (O.distname ? opts)
  predist <- getPrefval "predist"
  let resultfile = formerdir </> distname ++ ".tar.gz"
  withTempDir "darcsdist" $ \tempdir -> do
      setCurrentDirectory formerdir
      let ddir = toFilePath tempdir </> distname
      createDirectory ddir
      case patchSetMatch matchFlags of
        Just psm -> withCurrentDirectory ddir $ getRecordedUpToMatch repository psm
        Nothing -> createPartialsPristineDirectoryTree repository [anchoredRoot] (toFilePath ddir)
      ec <- case predist of Nothing -> return ExitSuccess
                            Just pd -> system pd
      if ec == ExitSuccess
        then do
          withCurrentDirectory ddir $
            when
              (F.setScriptsExecutable ? opts == O.YesSetScriptsExecutable)
              setScriptsExecutable
          doDist opts tempdir distname resultfile
        else do
          putStrLn "Dist aborted due to predist failure"
          exitWith ec


-- | This function performs the actual distribution action itself.
-- NB - it does /not/ perform the pre-dist, that should already
-- have completed successfully before this is invoked.
doDist :: [DarcsFlag] -> AbsolutePath -> String -> FilePath -> IO ()
doDist opts tempdir name resultfile = do
    setCurrentDirectory (toFilePath tempdir)
    entries <- pack "." [name]
    putVerbose opts $ vcat $ map (text . entryPath) entries
    writeFile resultfile $ compress $ write entries
    putInfo opts $ text $ "Created dist as " ++ resultfile


getDistName :: FilePath -> Maybe String -> FilePath
getDistName _ (Just dn) = takeFileName dn
getDistName currentDirectory _ = takeFileName currentDirectory

doFastZip :: [DarcsFlag] -> IO ()
doFastZip opts = do
  currentdir <- getCurrentDirectory
  let distname = getDistName currentdir (O.distname ? opts)
  let resultfile = currentdir </> distname ++ ".zip"
  doFastZip' opts currentdir (writeFile resultfile)
  putInfo opts $ text $ "Created " ++ resultfile

doFastZip' :: [DarcsFlag]              -- ^ Flags/options
           -> FilePath                 -- ^ The path to the repository
           -> (BL.ByteString -> IO a)  -- ^ An action to perform on the archive contents
           -> IO a
doFastZip' opts path act = withRepositoryLocation (useCache ? opts) path $ RepoJob $ \repo -> do
  when (F.setScriptsExecutable ? opts == O.YesSetScriptsExecutable) $
    putStrLn "WARNING: Zip archives cannot store executable flag."  
  let distname = getDistName path (O.distname ? opts)
  i <- fetchFilePS (path </> hashedInventoryPath) Uncachable
  pristine <- pathsAndContents (distname ++ "/") (repoCache repo) (peekPristineHash i)
  epochtime <- toSeconds `fmap` getCurrentTime
  let entries = [ toEntry filepath epochtime (toLazy contents) | (filepath,contents) <- pristine ]
  let archive = foldr addEntryToArchive emptyArchive entries
  act (fromArchive archive)


toLazy :: B.ByteString -> BL.ByteString
toLazy bs = BL.fromChunks [bs]
