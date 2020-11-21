{-|
License : GPL-2

Packs are an optimization that enable faster repository cloning over HTTP.
A pack is actually a @tar.gz@ file that contains many files that would otherwise
have to be transfered one by one (which is much slower over HTTP).

Two packs are created at the same time by 'createPacks':

  1. The basic pack, contains the latest recorded version of the working tree.
  2. The patches pack, contains the set of patches of the repository.

The paths of these files are @_darcs\/packs\/basic.tar.gz@ and
@_darcs\/packs\/patches.tar.gz@. There is also @_darcs\/packs\/pristine@ which
indicates the pristine hash at the moment of the creation of the packs. This
last file is useful to determine whether the basic pack is in sync with the
current pristine of the repository.
-}

module Darcs.Repository.Packs
    ( fetchAndUnpackBasic
    , fetchAndUnpackPatches
    , packsDir
    , createPacks
    ) where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath )
import Codec.Compression.GZip as GZ ( compress, decompress )
import Control.Concurrent.Async ( withAsync )
import Control.Exception ( Exception, IOException, throwIO, catch, finally )
import Control.Monad ( void, when, unless )
import System.IO.Error ( isAlreadyExistsError )
import System.IO.Unsafe ( unsafeInterleaveIO )

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List ( isPrefixOf, sort )

import System.Directory ( createDirectoryIfMissing
                        , renameFile
                        , removeFile
                        , doesFileExist
                        , getModificationTime
                        , listDirectory
                        )
import System.FilePath ( (</>)
                       , (<.>)
                       , takeFileName
                       , splitPath
                       , joinPath
                       , takeDirectory
                       )
import System.Posix.Files ( createLink )

import Darcs.Prelude

import Darcs.Util.ByteString ( gzReadFilePS )
import Darcs.Util.Lock ( withTemp )
import Darcs.Util.External ( Cachable(..), fetchFileLazyPS )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Progress ( debugMessage, progressList )

import Darcs.Patch ( IsRepoType, RepoPatch )
import Darcs.Patch.PatchInfoAnd ( extractHash )
import Darcs.Patch.Progress ( progressFL )
import Darcs.Patch.Witnesses.Ordered ( mapFL )
import Darcs.Patch.Set ( patchSet2FL )

import Darcs.Repository.Traverse ( listInventories )
import Darcs.Repository.InternalTypes ( Repository )
import Darcs.Repository.Hashed ( readRepo )
import Darcs.Repository.Inventory ( getValidHash )
import Darcs.Repository.Format
    ( identifyRepoFormat, formatHas, RepoProperty ( HashedInventory ) )
import Darcs.Repository.Cache ( fetchFileUsingCache
                              , HashedDir(..)
                              , Cache
                              , closestWritableDirectory
                              , hashedDir
                              , bucketFolder
                              )
import Darcs.Repository.Old ( oldRepoFailMsg )
import Darcs.Repository.Pristine ( readHashedPristineRoot )

packsDir, basicPack, patchesPack :: String
packsDir     = "packs"
basicPack    = "basic.tar.gz"
patchesPack  = "patches.tar.gz"

fetchAndUnpack :: FilePath
               -> HashedDir
               -> Cache
               -> FilePath
               -> IO ()
fetchAndUnpack filename dir cache remote = do
  unpackTar cache dir . Tar.read . GZ.decompress =<<
    fetchFileLazyPS (remote </> darcsdir </> packsDir </> filename) Uncachable

fetchAndUnpackPatches :: [String] -> Cache -> FilePath -> IO ()
fetchAndUnpackPatches paths cache remote =
  -- Patches pack can miss some new patches of the repository.
  -- So we download pack asynchonously and alway do a complete pass
  -- of individual patch files.
  withAsync (fetchAndUnpack patchesPack HashedInventoriesDir cache remote) $ \_ -> do
  fetchFilesUsingCache cache HashedPatchesDir paths

fetchAndUnpackBasic :: Cache -> FilePath -> IO ()
fetchAndUnpackBasic = fetchAndUnpack basicPack HashedPristineDir

unpackTar :: Exception e => Cache -> HashedDir -> Tar.Entries e -> IO ()
unpackTar _ _   Tar.Done = return ()
unpackTar _ _   (Tar.Fail e) = throwIO e
unpackTar c dir (Tar.Next e es) = case Tar.entryContent e of
  Tar.NormalFile bs _ -> do
    let p = Tar.entryPath e
    if "meta-" `isPrefixOf` takeFileName p
      then unpackTar c dir es -- just ignore them
      else do
        ex <- doesFileExist p
        if ex
          then debugMessage $ "TAR thread: exists " ++ p ++ "\nStopping TAR thread."
          else do
            if p == darcsdir </> "hashed_inventory"
              then writeFile' Nothing p bs
              else writeFile' (closestWritableDirectory c) p $ GZ.compress bs
            debugMessage $ "TAR thread: GET " ++ p
            unpackTar c dir es
  _ -> fail "Unexpected non-file tar entry"
 where
  writeFile' Nothing path content = withTemp $ \tmp -> do
    BLC.writeFile tmp content
    renameFile tmp path
  writeFile' (Just ca) path content = do
    let fileFullPath = case splitPath path of
          _:hDir:hFile:_  -> joinPath [ca, hDir, bucketFolder hFile, hFile]
          _               -> fail "Unexpected file path"
    createDirectoryIfMissing True $ takeDirectory path
    createLink fileFullPath path `catch` (\(ex :: IOException) -> do
      if isAlreadyExistsError ex then
        return () -- so much the better
      else
        -- ignore cache if we cannot link
        writeFile' Nothing path content)

-- | Similar to @'mapM_' ('void' 'fetchFileUsingCache')@, exepts
-- it stops execution if file it's going to fetch already exists.
fetchFilesUsingCache :: Cache -> HashedDir -> [FilePath] -> IO ()
fetchFilesUsingCache cache dir = mapM_ go where
  go path = do
    ex <- doesFileExist $ darcsdir </> hashedDir dir </> path
    if ex
     then debugMessage $ "FILE thread: exists " ++ path
     else void $ fetchFileUsingCache cache dir path

-- | Create packs from the current recorded version of the repository.
createPacks :: (IsRepoType rt, RepoPatch p)
            => Repository rt p wR wU wT -> IO ()
createPacks repo = flip finally (mapM_ removeFileIfExists
  [ darcsdir </> "meta-filelist-inventories"
  , darcsdir </> "meta-filelist-pristine"
  , basicTar <.> "part"
  , patchesTar <.> "part"
  ]) $ do
  rf <- identifyRepoFormat "."
  -- function is exposed in API so could be called on non-hashed repo
  unless (formatHas HashedInventory rf) $ fail oldRepoFailMsg
  createDirectoryIfMissing False (darcsdir </> packsDir)
  -- pristine hash
  Just hash <- readHashedPristineRoot repo
  writeFile ( darcsdir </> packsDir </> "pristine" ) $ getValidHash hash
  -- pack patchesTar
  ps <- mapFL hashedPatchFileName . progressFL "Packing patches" . patchSet2FL <$> readRepo repo
  is <- map ((darcsdir </> "inventories") </>) <$> listInventories
  writeFile (darcsdir </> "meta-filelist-inventories") . unlines $
    map takeFileName is
  -- Note: tinkering with zlib's compression parameters does not make
  -- any noticeable difference in generated archive size;
  -- switching to bzip2 would provide ~25% gain OTOH.
  BLC.writeFile (patchesTar <.> "part") . GZ.compress . Tar.write =<<
    mapM fileEntry' ((darcsdir </> "meta-filelist-inventories") : ps ++ reverse is)
  renameFile (patchesTar <.> "part") patchesTar
  -- pack basicTar
  pr <- sortByMTime =<< dirContents (darcsdir </> "pristine.hashed")
  writeFile (darcsdir </> "meta-filelist-pristine") . unlines $
    map takeFileName pr
  BLC.writeFile (basicTar <.> "part") . GZ.compress . Tar.write =<< mapM fileEntry' (
    [ darcsdir </> "meta-filelist-pristine"
    , darcsdir </> "hashed_inventory"
    ] ++ progressList "Packing pristine" (reverse pr))
  renameFile (basicTar <.> "part") basicTar
 where
  basicTar = darcsdir </> packsDir </> basicPack
  patchesTar = darcsdir </> packsDir </> patchesPack
  fileEntry' x = unsafeInterleaveIO $ do
    content <- BLC.fromChunks . return <$> gzReadFilePS x
    tp <- either fail return $ toTarPath False x
    return $ fileEntry tp content
  dirContents dir = map (dir </>) <$> listDirectory dir
  hashedPatchFileName x = case extractHash x of
    Left _ -> fail "unexpected unhashed patch"
    Right h -> darcsdir </> "patches" </> h
  sortByMTime xs = map snd . sort <$> mapM (\x -> (\t -> (t, x)) <$>
    getModificationTime x) xs
  removeFileIfExists x = do
    ex <- doesFileExist x
    when ex $ removeFile x
