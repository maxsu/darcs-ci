module Darcs.Repository.Pristine
    ( ApplyDir(..)
    , applyToHashedPristine
    , applyToTentativePristine
    , applyToTentativePristineCwd
    , readHashedPristineRoot
    , pokePristineHash
    , peekPristineHash
    , createPristineDirectoryTree
    , createPartialsPristineDirectoryTree
    , withRecorded
    , withTentative
    ) where

import Darcs.Prelude

import Control.Arrow ( (&&&) )
import Control.Exception ( catch, IOException )
import Control.Monad ( when )

import qualified Data.ByteString.Char8 as BC ( unpack, pack )

import System.Directory ( createDirectoryIfMissing )
import System.FilePath.Posix( (</>) )
import System.IO ( hPutStrLn, stderr )

import Darcs.Patch ( description )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Show ( ShowPatch )

import Darcs.Repository.Cache ( Cache, HashedDir(..), mkCache )
import Darcs.Repository.Flags ( Verbosity(..), WithWorkingDir(..) )
import Darcs.Repository.Format ( RepoProperty(HashedInventory), formatHas )
import Darcs.Repository.HashedIO ( cleanHashdir, copyHashed, copyPartialsHashed )
import Darcs.Repository.Inventory
import Darcs.Repository.InternalTypes
    ( Repository
    , repoCache
    , repoFormat
    , repoLocation
    , withRepoLocation
    )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.Paths
    ( hashedInventoryPath
    , pristineDirPath
    , tentativePristinePath
    )

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.External ( Cachable(Uncachable), fetchFilePS )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Hash ( Hash(..), encodeBase16 )
import Darcs.Util.Lock ( writeDocBinFile )
import Darcs.Util.Path ( AbsolutePath, AnchoredPath, toFilePath )
import Darcs.Util.Printer ( (<+>), putDocLn, text )
import Darcs.Util.Progress ( beginTedious, endTedious, debugMessage )
import Darcs.Util.Tree ( Tree, treeHash )
import Darcs.Util.Tree.Hashed
    ( decodeDarcsHash
    , decodeDarcsSize
    , hashedTreeIO
    , readDarcsHashed
    , readDarcsHashedNosize
    , writeDarcsHashed
    )


data ApplyDir = ApplyNormal | ApplyInverted

-- | 'applyToHashedPristine' takes a root hash, a patch @p@ and attempts to
-- apply the patch to the 'Tree' identified by @h@. If we encounter an old,
-- size-prefixed pristine, we first convert it to the non-size-prefixed format,
-- then apply the patch.
applyToHashedPristine :: (Apply p, ApplyState p ~ Tree)
                      => ApplyDir -> PristineHash -> p wX wY -> IO PristineHash
applyToHashedPristine dir h p = applyOrConvertOldPristineAndApply
  where
    applyOrConvertOldPristineAndApply =
        tryApply hash `catch` \(_ :: IOException) -> handleOldPristineAndApply

    hash = decodeDarcsHash $ BC.pack $ getValidHash h

    failOnMalformedRoot (SHA256 _) = return ()
    failOnMalformedRoot root = fail $ "Cannot handle hash: " ++ show root

    hash2root = mkValidHash . BC.unpack . encodeBase16

    tryApply :: Hash -> IO PristineHash
    tryApply root = do
        failOnMalformedRoot root
        -- Read a non-size-prefixed pristine, failing if we encounter one.
        tree <- readDarcsHashedNosize pristineDirPath root
        (_, updatedTree) <- case dir of
            ApplyNormal -> hashedTreeIO (apply p) tree pristineDirPath
            ApplyInverted -> hashedTreeIO (unapply p) tree pristineDirPath
        return $ hash2root $ treeHash updatedTree

    warn = "WARNING: Doing a one-time conversion of pristine format.\n"
           ++ "This may take a while. The new format is backwards-compatible."

    handleOldPristineAndApply = do
        hPutStrLn stderr warn
        inv <- gzReadFilePS hashedInventoryPath
        let oldroot = BC.pack $ getValidHash $ peekPristineHash inv
            oldrootSizeandHash = (decodeDarcsSize &&& decodeDarcsHash) oldroot
        -- Read the old size-prefixed pristine tree
        old <- readDarcsHashed pristineDirPath oldrootSizeandHash
        -- Write out the pristine tree as a non-size-prefixed pristine.
        root <- writeDarcsHashed old pristineDirPath
        let newroot = hash2root root
        -- Write out the new inventory.
        writeDocBinFile hashedInventoryPath $ pokePristineHash newroot inv
        cleanHashdir (mkCache []) HashedPristineDir [newroot]
        hPutStrLn stderr "Pristine conversion done..."
        -- Retry applying the patch, which should now succeed.
        tryApply root

-- | copyPristine copies a pristine tree into the current pristine dir,
--   and possibly copies a clean working tree.
--   The target is read from the passed-in dir/inventory name combination.
copyPristine :: Cache -> String -> String -> WithWorkingDir -> IO ()
copyPristine cache dir iname wwd = do
    i <- fetchFilePS (dir ++ "/" ++ iname) Uncachable
    debugMessage $ "Copying hashed pristine tree: " ++ getValidHash (peekPristineHash i)
    let tediousName = "Copying pristine"
    beginTedious tediousName
    copyHashed tediousName cache wwd $ peekPristineHash i
    endTedious tediousName

-- |applyToTentativePristine applies a patch @p@ to the tentative pristine
-- tree, and updates the tentative pristine hash
applyToTentativePristine :: (ApplyState q ~ Tree, Apply q, ShowPatch q)
                         => Repository rt p wR wU wT
                         -> ApplyDir
                         -> Verbosity
                         -> q wT wY
                         -> IO ()
applyToTentativePristine r dir verb p =
  withRepoLocation r $ do
    when (verb == Verbose) $
      putDocLn $ text "Applying to pristine..." <+> description p
    applyToTentativePristineCwd dir p

applyToTentativePristineCwd :: (ApplyState p ~ Tree, Apply p)
                            => ApplyDir
                            -> p wX wY
                            -> IO ()
applyToTentativePristineCwd dir p = do
    tentativePristine <- gzReadFilePS tentativePristinePath
    -- Extract the pristine hash from the tentativePristine file, using
    -- peekPristineHash (this is valid since we normally just extract the hash from the
    -- first line of an inventory file; we can pass in a one-line file that
    -- just contains said hash).
    let tentativePristineHash = peekPristineHash tentativePristine
    newPristineHash <- applyToHashedPristine dir tentativePristineHash p
    writeDocBinFile tentativePristinePath $
        pokePristineHash newPristineHash tentativePristine

-- | Used by the commands dist and diff
createPartialsPristineDirectoryTree :: Repository rt p wR wU wT
                                    -> [AnchoredPath]
                                    -> FilePath
                                    -> IO ()
createPartialsPristineDirectoryTree r paths target_dir
    | formatHas HashedInventory (repoFormat r) =
        do createDirectoryIfMissing True target_dir
           withCurrentDirectory target_dir $
            copyPartialsPristine (repoCache r) (repoLocation r) hashedInventoryPath
    | otherwise = fail oldRepoFailMsg
  where
    -- |copyPartialsPristine copies the pristine entries for a given list of
    -- filepaths.
    copyPartialsPristine cache repo_loc inv_name = do
        raw_inv <- fetchFilePS (repo_loc </> inv_name) Uncachable
        copyPartialsHashed cache (peekPristineHash raw_inv) paths

-- |readHashedPristineRoot attempts to read the pristine hash from the current
-- inventory, returning Nothing if it cannot do so.
readHashedPristineRoot :: Repository rt p wR wU wT -> IO (Maybe PristineHash)
readHashedPristineRoot r = withRepoLocation r $ do
    i <- (Just <$> gzReadFilePS hashedInventoryPath)
         `catch` (\(_ :: IOException) -> return Nothing)
    return $ peekPristineHash <$> i

-- | grab the pristine hash of _darcs/hash_inventory, and retrieve whole pristine tree,
--   possibly writing a clean working tree in the process.
createPristineDirectoryTree :: Repository rt p wR wU wT -> FilePath -> WithWorkingDir -> IO ()
createPristineDirectoryTree r reldir wwd
    | formatHas HashedInventory (repoFormat r) =
        do createDirectoryIfMissing True reldir
           withCurrentDirectory reldir $
              copyPristine (repoCache r) (repoLocation r) hashedInventoryPath wwd
    | otherwise = fail oldRepoFailMsg

withRecorded :: Repository rt p wR wU wT
             -> ((AbsolutePath -> IO a) -> IO a)
             -> (AbsolutePath -> IO a)
             -> IO a
withRecorded repository mk_dir f =
  mk_dir $ \d -> do
    createPristineDirectoryTree repository (toFilePath d) WithWorkingDir
    f d

withTentative :: Repository rt p wR wU wT
              -> ((AbsolutePath -> IO a) -> IO a)
              -> (AbsolutePath -> IO a)
              -> IO a
withTentative r mk_dir f
    | formatHas HashedInventory (repoFormat r) =
        mk_dir $ \d -> do copyPristine
                              (repoCache r)
                              (repoLocation r)
                              (darcsdir++"/tentative_pristine")
                              WithWorkingDir
                          f d
    | otherwise = fail oldRepoFailMsg
