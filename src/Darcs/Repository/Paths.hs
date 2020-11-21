-- everything here has type String/FilePath
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Darcs.Repository.Paths where

import Darcs.Prelude
import Darcs.Util.Global ( darcsdir )
import System.FilePath.Posix( (</>) )

makeDarcsdirPath :: String -> String
makeDarcsdirPath name = darcsdir </> name

-- | Location of the lock file.
lockPath = makeDarcsdirPath "lock"

-- | Location of the (one and only) head inventory.
hashedInventory = "hashed_inventory"
hashedInventoryPath = makeDarcsdirPath hashedInventory

-- | Location of the (one and only) tentative head inventory.
tentativeHashedInventory = "tentative_hashed_inventory"
tentativeHashedInventoryPath = makeDarcsdirPath tentativeHashedInventory

-- | Location of parent inventories.
inventoriesDir = "inventories"
inventoriesDirPath = makeDarcsdirPath inventoriesDir

-- | Location of pristine trees.
tentativePristinePath = makeDarcsdirPath "tentative_pristine"
pristineDir = "pristine.hashed"
pristineDirPath = makeDarcsdirPath pristineDir

-- | Location of patches.
patchesDir = "patches"
patchesDirPath = makeDarcsdirPath patchesDir

-- | Location of index files.
indexPath = darcsdir </> "index"
indexInvalidPath = darcsdir </> "index_invalid"

-- | Location of the rebase patch
rebasePath = makeDarcsdirPath "rebase"
tentativeRebasePath = makeDarcsdirPath "rebase.tentative"

-- | Location of format file
formatPath = makeDarcsdirPath "format"

-- | Location of pending files
pendingPath = patchesDirPath </> "pending"
tentativePendingPath = patchesDirPath </> "pending.tentative"
newPendingPath = patchesDirPath </> "pending.new"

-- | Location of unrevert bundle.
unrevertPath = patchesDirPath </> "unrevert"

-- | Location of old style (unhashed) files and directories.
oldPristineDirPath = makeDarcsdirPath "pristine"
oldCurrentDirPath = makeDarcsdirPath "current"
oldCheckpointDirPath = makeDarcsdirPath "checkpoints"
oldInventoryPath = makeDarcsdirPath "inventory"
oldTentativeInventoryPath = makeDarcsdirPath "tentative_inventory"
