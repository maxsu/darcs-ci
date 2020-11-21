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

{-# LANGUAGE OverloadedStrings #-}

module Darcs.UI.Commands.Optimize ( optimize ) where

import Darcs.Prelude

import Control.Monad ( when, unless, forM_ )
import Data.List ( nub )
import Data.Maybe ( fromJust, isJust )
import System.Directory
    ( listDirectory
    , doesDirectoryExist
    , renameFile
    , createDirectoryIfMissing
    , removeFile
    , getHomeDirectory
    , removeDirectoryRecursive
    )
import qualified Data.ByteString.Char8 as BC
import Darcs.UI.Commands ( DarcsCommand(..), nodefaults
                         , amInHashedRepository, amInRepository, putInfo
                         , normalCommand, withStdOpts )
import Darcs.UI.Completion ( noArgs )
import Darcs.Repository.Prefs ( getPreflist, getCaches, globalCacheDir )
import Darcs.Repository
    ( Repository
    , repoLocation
    , withRepoLock
    , RepoJob(..)
    , readRepo
    , reorderInventory
    , cleanRepository
    , replacePristine
    )
import Darcs.Repository.Job ( withOldRepoLock )
import Darcs.Repository.Identify ( findAllReposInDir )
import Darcs.Repository.Traverse
    ( diffHashLists
    , listInventoriesRepoDir
    , listPatchesLocalBucketed
    , specialPatches
    )
import Darcs.Repository.Inventory ( peekPristineHash )
import Darcs.Repository.Paths
    ( formatPath
    , hashedInventoryPath
    , inventoriesDir
    , inventoriesDirPath
    , oldCheckpointDirPath
    , oldCurrentDirPath
    , oldInventoryPath
    , oldPristineDirPath
    , oldTentativeInventoryPath
    , patchesDir
    , patchesDirPath
    , pristineDir
    , pristineDirPath
    , tentativePristinePath
    )
import Darcs.Repository.Packs ( createPacks )
import Darcs.Repository.HashedIO ( getHashedFiles )
import Darcs.Repository.Inventory ( getValidHash )
import Darcs.Patch.Witnesses.Ordered
     ( mapFL
     , bunchFL
     , lengthRL
     )
import Darcs.Patch ( IsRepoType, RepoPatch )
import Darcs.Patch.Set
    ( patchSet2RL
    , patchSet2FL
    , progressPatchSet
    )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Printer ( Doc, formatWords, text, wrapText, ($+$) )
import Darcs.Util.Lock
    ( maybeRelink
    , gzWriteAtomicFilePS
    , writeAtomicFilePS
    , removeFileMayNotExist
    , writeBinFile
    )
import Darcs.Util.File
    ( withCurrentDirectory
    , getRecursiveContents
    , doesDirectoryReallyExist
    )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Progress
    ( beginTedious
    , endTedious
    , tediousSize
    , debugMessage
    )
import Darcs.Util.Global ( darcsdir )

import System.FilePath.Posix
    ( takeExtension
    , (</>)
    , joinPath
    )
import Text.Printf ( printf )
import Darcs.UI.Flags
    (  DarcsFlag, useCache, umask )
import Darcs.UI.Options ( DarcsOption, (^), odesc, ocheck
                        , defaultFlags, parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags
    ( UpdatePending (..), DryRun ( NoDryRun ), UseCache (..), UMask (..)
    , WithWorkingDir(WithWorkingDir), PatchFormat(PatchFormat1) )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Repository.Cache ( hashedDir, bucketFolder,
                                HashedDir(HashedPristineDir) )
import Darcs.Repository.Format
    ( identifyRepoFormat
    , createRepoFormat
    , writeRepoFormat
    , formatHas
    , RepoProperty ( HashedInventory )
    )
import Darcs.Repository.PatchIndex
import Darcs.Repository.Hashed
    ( writeTentativeInventory
    , finalizeTentativeChanges
    )
import Darcs.Repository.Pristine
    ( ApplyDir(ApplyNormal)
    , applyToTentativePristineCwd
    )
import Darcs.Repository.State ( readRecorded )

import Darcs.Util.Tree
    ( Tree
    , TreeItem(..)
    , list
    , expand
    , emptyTree
    )
import Darcs.Util.Path( realPath, toFilePath, AbsolutePath )
import Darcs.Util.Tree.Plain( readPlainTree )
import Darcs.Util.Tree.Hashed
    ( writeDarcsHashed
    , decodeDarcsSize
    )

optimizeDescription :: String
optimizeDescription = "Optimize the repository."

optimizeHelp :: Doc
optimizeHelp = formatWords
  [ "The `darcs optimize` command modifies internal data structures of"
  , "the current repository in an attempt to reduce its resource requirements."
  ]
  $+$ "For further details see the descriptions of the subcommands."

optimize :: DarcsCommand
optimize = SuperCommand {
      commandProgramName = "darcs"
    , commandName = "optimize"
    , commandHelp = optimizeHelp
    , commandDescription = optimizeDescription
    , commandPrereq = amInRepository
    , commandSubCommands = [  normalCommand optimizeClean,
                              normalCommand optimizeHttp,
                              normalCommand optimizeReorder,
                              normalCommand optimizeEnablePatchIndex,
                              normalCommand optimizeDisablePatchIndex,
                              normalCommand optimizeCompress,
                              normalCommand optimizeUncompress,
                              normalCommand optimizeRelink,
                              normalCommand optimizePristine,
                              normalCommand optimizeUpgrade,
                              normalCommand optimizeGlobalCache
                           ]
    }

commonBasicOpts :: DarcsOption a (Maybe String -> a)
commonBasicOpts = O.repoDir

commonAdvancedOpts :: DarcsOption a (UMask -> a)
commonAdvancedOpts = O.umask

common :: DarcsCommand
common = DarcsCommand
    { commandProgramName = "darcs"
    , commandExtraArgs = 0
    , commandExtraArgHelp = []
    , commandPrereq =  amInHashedRepository
    , commandArgdefaults = nodefaults
    , commandName = undefined
    , commandHelp = undefined
    , commandDescription = undefined
    , commandCommand =  undefined
    , commandCompleteArgs = noArgs
    , commandAdvancedOptions = odesc commonAdvancedOpts
    , commandBasicOptions = odesc commonBasicOpts
    , commandDefaults = defaultFlags commonOpts
    , commandCheckOptions = ocheck commonOpts
    }
  where
    commonOpts = commonBasicOpts `withStdOpts` commonAdvancedOpts


optimizeClean :: DarcsCommand
optimizeClean = common
    { commandName = "clean"
    , commandDescription = "garbage collect pristine, inventories and patches"
    , commandHelp = optimizeHelpClean
    , commandCommand = optimizeCleanCmd
    }

optimizeCleanCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCleanCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      putInfo opts "Done cleaning repository!"

optimizeUpgrade :: DarcsCommand
optimizeUpgrade = common
    { commandName = "upgrade"
    , commandHelp = wrapText 80
        "Convert old-fashioned repositories to the current default hashed format."
    , commandDescription = "upgrade repository to latest compatible format"
    , commandPrereq = amInRepository
    , commandCommand = optimizeUpgradeCmd
    }

optimizeHttp :: DarcsCommand
optimizeHttp = common
    { commandName = "http"
    , commandHelp = optimizeHelpHttp
    , commandDescription = "optimize repository for getting over network"
    , commandCommand = optimizeHttpCmd
    }

optimizeHttpCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeHttpCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      createPacks repository
      putInfo opts "Done creating packs!"

optimizePristine :: DarcsCommand
optimizePristine = common
    { commandName = "pristine"
    , commandHelp = wrapText 80 $
        "This command updates the format of `"++pristineDirPath++
        "`, which was different\nbefore darcs 2.3.1."
    , commandDescription = "optimize hashed pristine layout"
    , commandCommand = optimizePristineCmd
    }

optimizePristineCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizePristineCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      doOptimizePristine opts repository
      putInfo opts "Done optimizing pristine!"

optimizeCompress :: DarcsCommand
optimizeCompress = common
    { commandName = "compress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "compress patches and inventories"
    , commandCommand = optimizeCompressCmd
    }

optimizeUncompress :: DarcsCommand
optimizeUncompress = common
    { commandName = "uncompress"
    , commandHelp = optimizeHelpCompression
    , commandDescription = "uncompress patches and inventories"
    , commandCommand = optimizeUncompressCmd
    }

optimizeCompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeCompressCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      optimizeCompression O.GzipCompression opts
      putInfo opts "Done optimizing by compression!"

optimizeUncompressCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUncompressCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      optimizeCompression O.NoCompression opts
      putInfo opts "Done optimizing by uncompression!"

optimizeCompression :: O.Compression -> [DarcsFlag] -> IO ()
optimizeCompression compression opts = do
    putInfo opts "Optimizing (un)compression of patches..."
    do_compress patchesDirPath
    putInfo opts "Optimizing (un)compression of inventories..."
    do_compress inventoriesDirPath
    where
      do_compress f = do
        isd <- doesDirectoryExist f
        if isd
          then withCurrentDirectory f $ do
                 fs <- filter (`notElem` specialPatches) <$> listDirectory "."
                 mapM_ do_compress fs
          else gzReadFilePS f >>=
               case compression of
                 O.GzipCompression -> gzWriteAtomicFilePS f
                 O.NoCompression -> writeAtomicFilePS f

optimizeEnablePatchIndex :: DarcsCommand
optimizeEnablePatchIndex = common
    { commandName = "enable-patch-index"
    , commandHelp = formatWords
        [ "Build the patch index, an internal data structure that accelerates"
        , "commands that need to know what patches touch a given file. Such as"
        , "annotate and log."
        ]
    , commandDescription = "Enable patch index"
    , commandCommand = optimizeEnablePatchIndexCmd
    }

optimizeDisablePatchIndex :: DarcsCommand
optimizeDisablePatchIndex = common
    { commandName = "disable-patch-index"
    , commandHelp = wrapText 80
        "Delete and stop maintaining the patch index from the repository."
    , commandDescription = "Disable patch index"
    , commandCommand = optimizeDisablePatchIndexCmd
    }

optimizeEnablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeEnablePatchIndexCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      ps <- readRepo repository
      createOrUpdatePatchIndexDisk repository ps
      putInfo opts "Done enabling patch index!"

optimizeDisablePatchIndexCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeDisablePatchIndexCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repo -> do
      deletePatchIndex (repoLocation repo)
      putInfo opts "Done disabling patch index!"

optimizeReorder :: DarcsCommand
optimizeReorder = common
    { commandName = "reorder"
    , commandHelp = formatWords
        [ "This command moves recent patches (those not included in"
        , "the latest tag) to the \"front\", reducing the amount that a typical"
        , "remote command needs to download.  It should also reduce the CPU time"
        , "needed for some operations."
        ]
    , commandDescription = "reorder the patches in the repository"
    , commandCommand = optimizeReorderCmd
    }

optimizeReorderCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeReorderCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      reorderInventory repository (O.compress ? opts)
      putInfo opts "Done reordering!"

optimizeRelink :: DarcsCommand
optimizeRelink = common
    { commandName = "relink"
    , commandHelp = optimizeHelpRelink 
    , commandDescription = "relink random internal data to a sibling"
    , commandCommand = optimizeRelinkCmd
    , commandAdvancedOptions = odesc commonAdvancedOpts
    , commandBasicOptions = odesc optimizeRelinkBasicOpts
    , commandDefaults = defaultFlags optimizeRelinkOpts
    , commandCheckOptions = ocheck optimizeRelinkOpts
    }
  where
    optimizeRelinkBasicOpts = commonBasicOpts ^ O.siblings
    optimizeRelinkOpts = optimizeRelinkBasicOpts `withStdOpts` commonAdvancedOpts

optimizeRelinkCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeRelinkCmd _ opts _ =
    withRepoLock NoDryRun (useCache ? opts) YesUpdatePending (umask ? opts) $
    RepoJob $ \repository -> do
      cleanRepository repository -- garbage collect pristine.hashed, inventories and patches directories
      doRelink opts
      putInfo opts "Done relinking!"

optimizeHelpHttp :: Doc
optimizeHelpHttp = formatWords
  [ "Using this option creates 'repository packs' that can dramatically"
  , "speed up performance when a user does a `darcs clone` of the repository"
  , "over HTTP. To make use of packs, the clients must have a darcs of at"
  , "least version 2.10."
  ]

optimizeHelpClean :: Doc
optimizeHelpClean = formatWords
  [ "Darcs normally does not delete hashed files that are no longer"
  , "referenced by the current repository state. This command can be"
  , "use to get rid of these files to save some disk space."
  ]

optimizeHelpCompression :: Doc
optimizeHelpCompression =
  formatWords
  [ "By default patches are compressed with zlib (RFC 1951) to reduce"
  , "storage (and download) size.  In exceptional circumstances, it may be"
  , "preferable to avoid compression.  In this case the `--dont-compress`"
  , "option can be used (e.g. with `darcs record`) to avoid compression."
  ]
  $+$ formatWords
  [ "The `darcs optimize uncompress` and `darcs optimize compress`"
  , "commands can be used to ensure existing patches in the current"
  , "repository are respectively uncompressed or compressed."
  ]

optimizeHelpRelink :: Doc
optimizeHelpRelink = 
  formatWords
  [ "The `darcs optimize relink` command hard-links patches that the"
  , "current repository has in common with its peers.  Peers are those"
  , "repositories listed in `_darcs/prefs/sources`, or defined with the"
  , "`--sibling` option (which can be used multiple times)."
  ]
  $+$ formatWords
  [ "Darcs uses hard-links automatically, so this command is rarely needed."
  , "It is most useful if you used `cp -r` instead of `darcs clone` to copy a"
  , "repository, or if you pulled the same patch from a remote repository"
  , "into multiple local repositories."
  ]

doOptimizePristine :: [DarcsFlag] -> Repository rt p wR wU wT -> IO ()
doOptimizePristine opts repo = do
    inv <- BC.readFile hashedInventoryPath
    let linesInv = BC.split '\n' inv
    case linesInv of
      [] -> return ()
      (pris_line:_) ->
          let size = decodeDarcsSize $ BC.drop 9 pris_line
           in when (isJust size) $ do putInfo opts "Optimizing hashed pristine..."
                                      readRecorded repo >>= replacePristine repo
                                      cleanRepository repo

doRelink :: [DarcsFlag] -> IO ()
doRelink opts =
    do let some_siblings = parseFlags O.siblings opts
       defrepolist <- getPreflist "defaultrepo"
       let siblings = map toFilePath some_siblings ++ defrepolist
       if null siblings
          then putInfo opts "No siblings -- no relinking done."
          else do debugMessage "Relinking patches..."
                  patch_tree <- expand =<< readPlainTree patchesDirPath
                  let patches = [ realPath p | (p, File _) <- list patch_tree ]
                  maybeRelinkFiles siblings patches patchesDirPath
                  debugMessage "Done relinking."

maybeRelinkFiles :: [String] -> [String] -> String -> IO ()
maybeRelinkFiles src dst dir =
    mapM_ (maybeRelinkFile src . ((dir ++ "/") ++)) dst

maybeRelinkFile :: [String] -> String -> IO ()
maybeRelinkFile [] _ = return ()
maybeRelinkFile (h:t) f =
    do done <- maybeRelink (h ++ "/" ++ f) f
       unless done $
           maybeRelinkFile t f

-- Only 'optimize' commands that works on old-fashionned repositories
optimizeUpgradeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeUpgradeCmd _ opts _ = do
  rf <- identifyRepoFormat "."
  debugMessage "Found our format"
  if formatHas HashedInventory rf
     then putInfo opts "No action taken because this repository already is hashed."
     else do putInfo opts "Upgrading to hashed..."
             withOldRepoLock $ RepoJob actuallyUpgradeFormat

actuallyUpgradeFormat
  :: (IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
  => Repository rt p wR wU wT -> IO ()
actuallyUpgradeFormat repository = do
  -- convert patches/inventory
  patches <- readRepo repository
  let k = "Hashing patch"
  beginTedious k
  tediousSize k (lengthRL $ patchSet2RL patches)
  let patches' = progressPatchSet k patches
  -- darcs optimize subcommands do not support
  -- the --no-cache option, so use default
  cache <- getCaches YesUseCache "."
  let compressDefault = O.compress ? []
  writeTentativeInventory cache compressDefault patches'
  endTedious k
  -- convert pristine by applying patches
  -- the faster alternative would be to copy pristine, but the apply method
  -- is more reliable
  -- TODO we should do both and then comapre them
  let patchesToApply = progressFL "Applying patch" $ patchSet2FL patches'
  createDirectoryIfMissing False $ darcsdir </> hashedDir HashedPristineDir
  -- We ignore the returned root hash, we don't use it.
  _ <- writeDarcsHashed emptyTree $ darcsdir </> hashedDir HashedPristineDir
  writeBinFile tentativePristinePath ""
  sequence_ $
    mapFL (applyToTentativePristineCwd ApplyNormal) $
    bunchFL 100 patchesToApply
  -- now make it official
  finalizeTentativeChanges repository compressDefault
  writeRepoFormat (createRepoFormat PatchFormat1 WithWorkingDir) formatPath
  -- clean out old-fashioned junk
  debugMessage "Cleaning out old-fashioned repository files..."
  removeFileMayNotExist oldInventoryPath
  removeFileMayNotExist oldTentativeInventoryPath
  removeDirectoryRecursive oldPristineDirPath
    `catchall` removeDirectoryRecursive oldCurrentDirPath
  rmGzsIn patchesDirPath
  rmGzsIn inventoriesDirPath
  hasCheckPoints <- doesDirectoryExist oldCheckpointDirPath
  when hasCheckPoints $ removeDirectoryRecursive oldCheckpointDirPath
 where
  rmGzsIn dir =
    withCurrentDirectory dir $ do
      gzs <- filter ((== ".gz") . takeExtension) `fmap` listDirectory "."
      mapM_ removeFile gzs

optimizeBucketed :: [DarcsFlag] -> IO ()
optimizeBucketed opts = do
  putInfo opts "Migrating global cache to bucketed format."
  gCacheDir <- globalCacheDir

  case gCacheDir of
    Nothing -> fail "New global cache doesn't exist."
    Just gCacheDir' -> do
      let gCachePristineDir = joinPath [gCacheDir', pristineDir]
          gCacheInventoriesDir = joinPath [gCacheDir', inventoriesDir]
          gCachePatchesDir = joinPath [gCacheDir', patchesDir]
      debugMessage "Making bucketed cache from new cache."
      toBucketed gCachePristineDir gCachePristineDir
      toBucketed gCacheInventoriesDir gCacheInventoriesDir
      toBucketed gCachePatchesDir gCachePatchesDir
      putInfo opts "Done making bucketed cache!"
  where
    toBucketed :: FilePath -> FilePath -> IO ()
    toBucketed src dest = do
      srcExist <- doesDirectoryExist src
      if srcExist
        then  do
                debugMessage $ "Making " ++ src ++ " bucketed in " ++ dest
                forM_ subDirSet $ \subDir ->
                  createDirectoryIfMissing True (dest </> subDir)
                fileNames <- listDirectory src
                forM_ fileNames $ \file -> do
                  exists <- doesDirectoryReallyExist (src </> file)
                  if not $ exists
                    then renameFile' src dest file
                    else return ()
        else do
          debugMessage $ show src ++ " didn't exist, doing nothing."
          return ()

    renameFile' :: FilePath -> FilePath -> FilePath -> IO ()
    renameFile' s d f = renameFile (s </> f) (joinPath [d, bucketFolder f, f])

    subDirSet :: [String]
    subDirSet = map toStrHex [0..255]

    toStrHex :: Int -> String
    toStrHex = printf "%02x"


optimizeGlobalCache :: DarcsCommand
optimizeGlobalCache = common
    { commandName = "cache"
    , commandExtraArgs            = -1
    , commandExtraArgHelp         = [ "<DIRECTORY> ..." ]
    , commandHelp = optimizeHelpGlobalCache
    , commandDescription = "garbage collect global cache"
    , commandCommand = optimizeGlobalCacheCmd
    , commandPrereq = \_ -> return $ Right ()
    }

optimizeHelpGlobalCache :: Doc
optimizeHelpGlobalCache = formatWords
  [ "This command deletes obsolete files within the global cache."
  , "It takes one or more directories as arguments, and recursively"
  , "searches all repositories within these directories. Then it deletes"
  , "all files in the global cache not belonging to these repositories."
  , "When no directory is given, it searches repositories in the user's"
  , "home directory."
  ]
  $+$ formatWords
  [ "It also automatically migrates the global cache to the (default)"
  , "bucketed format."
  ]

optimizeGlobalCacheCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
optimizeGlobalCacheCmd _ opts args = do
  optimizeBucketed opts
  home <- getHomeDirectory
  let args' = if null args then [home] else args
  cleanGlobalCache args' opts
  putInfo opts "Done cleaning global cache!"

cleanGlobalCache :: [String] -> [DarcsFlag] -> IO ()
cleanGlobalCache dirs opts = do
  putInfo opts "\nLooking for repositories in the following directories:"
  putInfo opts $ text $ unlines dirs
  gCacheDir' <- globalCacheDir
  repoPaths'  <- mapM findAllReposInDir dirs

  putInfo opts "Finished listing repositories."

  let repoPaths         = nub $ concat repoPaths'
      gCache            = fromJust gCacheDir'
      gCacheInvDir      = gCache </> inventoriesDir
      gCachePatchesDir  = gCache </> patchesDir
      gCachePristineDir = gCache </> pristineDir

  createDirectoryIfMissing True gCacheInvDir
  createDirectoryIfMissing True gCachePatchesDir
  createDirectoryIfMissing True gCachePristineDir

  remove listInventoriesRepoDir gCacheInvDir repoPaths
  remove (listPatchesLocalBucketed gCache . (</> darcsdir)) gCachePatchesDir repoPaths
  remove getPristine gCachePristineDir repoPaths

  where
  remove fGetFiles cacheSubDir repoPaths = do
    s1 <- mapM fGetFiles repoPaths
    s2 <- getRecursiveContents cacheSubDir
    remove' cacheSubDir s2 (concat s1)

  remove' :: String -> [String] -> [String] -> IO ()
  remove' dir s1 s2 =
    mapM_ (removeFileMayNotExist . (\hashedFile ->
      dir </> bucketFolder hashedFile </> hashedFile))
      (diffHashLists s1 s2)

  getPristine :: String -> IO [String]
  getPristine repoDir = do
    i <- gzReadFilePS (repoDir </> hashedInventoryPath)
    getHashedFiles (repoDir </> pristineDirPath) [getValidHash $ peekPristineHash i]
