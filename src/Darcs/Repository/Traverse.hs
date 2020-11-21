module Darcs.Repository.Traverse
    ( cleanInventories
    , cleanPatches
    , cleanPristine
    , cleanRepository
    , diffHashLists
    , listInventories
    , listInventoriesLocal
    , listInventoriesRepoDir
    , listPatchesLocalBucketed
    , specialPatches
    ) where

import Darcs.Prelude

import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.Set as Set

import System.Directory ( listDirectory )
import System.FilePath.Posix( (</>) )

import Darcs.Repository.Cache ( HashedDir(..), bucketFolder )
import Darcs.Repository.HashedIO ( cleanHashdir )
import Darcs.Repository.Inventory
    ( Inventory(..)
    , emptyInventory
    , getValidHash
    , inventoryPatchNames
    , parseInventory
    , peekPristineHash
    , skipPristineHash
    )
import Darcs.Repository.InternalTypes
    ( Repository
    , repoCache
    , withRepoLocation
    )
import Darcs.Repository.Paths
    ( hashedInventory
    , hashedInventoryPath
    , inventoriesDir
    , inventoriesDirPath
    , patchesDirPath
    )
import Darcs.Repository.Prefs ( globalCacheDir )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Exception ( ifDoesNotExistError )
import Darcs.Util.Global ( darcsdir, debugMessage )
import Darcs.Util.Lock ( removeFileMayNotExist )


cleanRepository :: Repository rt p wR wU wT -> IO ()
cleanRepository r = cleanPristine r >> cleanInventories r >> cleanPatches r

-- | The way patchfiles, inventories, and pristine trees are stored.
-- 'PlainLayout' means all files are in the same directory. 'BucketedLayout'
-- means we create a second level of subdirectories, such that all files whose
-- hash starts with the same two letters are in the same directory.
-- Currently, only the global cache uses 'BucketedLayout' while repositories
-- use the 'PlainLayout'.
data DirLayout = PlainLayout | BucketedLayout

-- | Remove unreferenced entries in the pristine cache.
cleanPristine :: Repository rt p wR wU wT -> IO ()
cleanPristine r = withRepoLocation r $ do
    debugMessage "Cleaning out the pristine cache..."
    i <- gzReadFilePS hashedInventoryPath
    cleanHashdir (repoCache r) HashedPristineDir [peekPristineHash i]

-- | Set difference between two lists of hashes.
diffHashLists :: [String] -> [String] -> [String]
diffHashLists xs ys = from_set $ (to_set xs) `Set.difference` (to_set ys)
  where
    to_set = Set.fromList . map BC.pack
    from_set = map BC.unpack . Set.toList

-- | Remove unreferenced files in the inventories directory.
cleanInventories :: Repository rt p wR wU wT -> IO ()
cleanInventories _ = do
    debugMessage "Cleaning out inventories..."
    hs <- listInventoriesLocal
    fs <- ifDoesNotExistError [] $ listDirectory inventoriesDirPath
    mapM_ (removeFileMayNotExist . (inventoriesDirPath </>))
        (diffHashLists fs hs)

-- FIXME this is ugly, these files should be directly under _darcs
-- since they are not hashed. And 'unrevert' isn't even a real patch but
-- a patch bundle.

-- | List of special patch files that may exist in the directory
-- _darcs/patches/. We must not clean those.
specialPatches :: [FilePath]
specialPatches = ["unrevert", "pending", "pending.tentative"]

-- | Remove unreferenced files in the patches directory.
cleanPatches :: Repository rt p wR wU wT -> IO ()
cleanPatches _ = do
    debugMessage "Cleaning out patches..."
    hs <- (specialPatches ++) <$> listPatchesLocal PlainLayout darcsdir darcsdir
    fs <- ifDoesNotExistError [] (listDirectory patchesDirPath)
    mapM_ (removeFileMayNotExist . (patchesDirPath </>)) (diffHashLists fs hs)

-- | Return a list of the inventories hashes.
-- The first argument can be readInventory or readInventoryLocal.
-- The second argument specifies whether the files are expected
-- to be stored in plain or in bucketed format.
-- The third argument is the directory of the parent inventory files.
-- The fourth argument is the directory of the head inventory file.
listInventoriesWith
  :: (FilePath -> IO Inventory)
  -> DirLayout
  -> String -> String -> IO [String]
listInventoriesWith readInv dirformat baseDir startDir = do
    mbStartingWithInv <- getStartingWithHash startDir hashedInventory
    followStartingWiths mbStartingWithInv
  where
    getStartingWithHash dir file = inventoryParent <$> readInv (dir </> file)

    invDir = baseDir </> inventoriesDir
    nextDir dir = case dirformat of
        BucketedLayout -> invDir </> bucketFolder dir
        PlainLayout -> invDir

    followStartingWiths Nothing = return []
    followStartingWiths (Just hash) = do
        let startingWith = getValidHash hash
        mbNextInv <- getStartingWithHash (nextDir startingWith) startingWith
        (startingWith :) <$> followStartingWiths mbNextInv

-- | Return a list of the inventories hashes.
-- This function attempts to retrieve missing inventory files from the cache.
listInventories :: IO [String]
listInventories =
    listInventoriesWith readInventory PlainLayout darcsdir darcsdir

-- | Return inventories hashes by following the head inventory.
-- This function does not attempt to retrieve missing inventory files.
listInventoriesLocal :: IO [String]
listInventoriesLocal =
    listInventoriesWith readInventoryLocal PlainLayout darcsdir darcsdir

-- | Return a list of the inventories hashes.
-- The argument @repoDir@ is the directory of the repository from which
-- we are going to read the head inventory file.
-- The rest of hashed files are read from the global cache.
listInventoriesRepoDir :: String -> IO [String]
listInventoriesRepoDir repoDir = do
    gCacheDir' <- globalCacheDir
    let gCacheInvDir = fromJust gCacheDir'
    listInventoriesWith
        readInventoryLocal
        BucketedLayout
        gCacheInvDir
        (repoDir </> darcsdir)

-- | Return a list of the patch filenames, extracted from inventory
-- files, by starting with the head inventory and then following the
-- chain of parent inventories.
--
-- This function does not attempt to download missing inventory files.
--
-- * The first argument specifies whether the files are expected
--   to be stored in plain or in bucketed format.
-- * The second argument is the directory of the parent inventory.
-- * The third argument is the directory of the head inventory.
listPatchesLocal :: DirLayout -> String -> String -> IO [String]
listPatchesLocal dirformat baseDir startDir = do
  inventory <- readInventory (startDir </> hashedInventory)
  followStartingWiths
    (inventoryParent inventory)
    (inventoryPatchNames inventory)
  where
    invDir = baseDir </> inventoriesDir
    nextDir dir =
      case dirformat of
        BucketedLayout -> invDir </> bucketFolder dir
        PlainLayout -> invDir
    followStartingWiths Nothing patches = return patches
    followStartingWiths (Just hash) patches = do
      let startingWith = getValidHash hash
      inv <- readInventoryLocal (nextDir startingWith </> startingWith)
      (patches ++) <$>
        followStartingWiths (inventoryParent inv) (inventoryPatchNames inv)

-- |listPatchesLocalBucketed is similar to listPatchesLocal, but
-- it read the inventory directory under @darcsDir@ in bucketed format.
listPatchesLocalBucketed :: String -> String -> IO [String]
listPatchesLocalBucketed = listPatchesLocal BucketedLayout

-- | Read the given inventory file if it exist, otherwise return an empty
-- inventory. Used when we expect that some inventory files may be missing.
-- Still fails with an error message if file cannot be parsed.
readInventoryLocal :: FilePath -> IO Inventory
readInventoryLocal path =
  ifDoesNotExistError emptyInventory $ readInventory path

-- | Read an inventory from a file. Fails with an error message if
-- file is not there or cannot be parsed.
readInventory :: FilePath -> IO Inventory
readInventory path = do
  -- FIXME we should check the hash (if this is a hashed file)
  inv <- skipPristineHash <$> gzReadFilePS path
  case parseInventory inv of
    Right r -> return r
    Left e -> fail $ unlines [unwords ["parse error in file", path], e]
