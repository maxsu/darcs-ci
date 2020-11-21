--  Copyright (C) 2009-2011 Petr Rockai
--
--  BSD3

-- | A few darcs-specific utility functions. These are used for reading and
-- writing darcs and darcs-compatible hashed trees.
module Darcs.Util.Tree.Hashed
    ( -- * Obtaining Trees.
    --
    -- | Please note that Trees obtained this way will contain Stub
    -- items. These need to be executed (they are IO actions) in order to be
    -- accessed. Use 'expand' to do this. However, many operations are
    -- perfectly fine to be used on a stubbed Tree (and it is often more
    -- efficient to do everything that can be done before expanding a Tree).
      readDarcsHashed
    -- * Writing trees.
    , writeDarcsHashed
    -- * Interact with hashed tree
    , hashedTreeIO
    -- * Other
    , readDarcsHashedDir
    , readDarcsHashedNosize
    , darcsAddMissingHashes
    , darcsLocation
    , darcsTreeHash
    , decodeDarcsHash
    , decodeDarcsSize
    , darcsUpdateHashes
    ) where

import System.FilePath ( (</>) )

import System.Directory( doesFileExist )
import Codec.Compression.GZip( decompress, compress )

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.List( sortBy )
import Data.Maybe( fromJust, isJust )
import Control.Monad.State.Strict (liftIO,when,unless)

import Darcs.Prelude

import Darcs.Util.ByteString (FileSegment, readSegment)
import Darcs.Util.Hash (Hash(..), decodeBase16, encodeBase16, sha256)
import Darcs.Util.Path (Name, decodeWhiteName, encodeWhiteName)
import Darcs.Util.Progress (debugMessage)
import Darcs.Util.Tree
    ( Blob(..)
    , ItemType(..)
    , Tree(..)
    , TreeItem(..)
    , addMissingHashes
    , expand
    , itemHash
    , list
    , listImmediate
    , makeTreeWithHash
    , readBlob
    , updateSubtrees
    , updateTree
    )
import Darcs.Util.Tree.Monad (TreeIO, initialState, runTreeMonad)

---------------------------------------------------------------------
-- Utilities for coping with the darcs directory format.
--

decodeDarcsHash :: BC.ByteString -> Hash
decodeDarcsHash bs = case BC.split '-' bs of
                       [s, h] | BC.length s == 10 -> decodeBase16 h
                       _ -> decodeBase16 bs

decodeDarcsSize :: BC.ByteString -> Maybe Int
decodeDarcsSize bs = case BC.split '-' bs of
                       [s, _] | BC.length s == 10 ->
                                  case reads (BC.unpack s) of
                                    [(x, _)] -> Just x
                                    _ -> Nothing
                       _ -> Nothing

darcsLocation :: FilePath -> (Maybe Int, Hash) -> FileSegment
darcsLocation dir (s,h) = case hash of
                            "" -> error "darcsLocation: invalid hash"
                            _ -> (dir </> prefix s ++ hash, Nothing)
    where prefix Nothing = ""
          prefix (Just s') = formatSize s' ++ "-"
          formatSize s' = let n = show s' in replicate (10 - length n) '0' ++ n
          hash = showHash h

----------------------------------------------
-- Darcs directory format.
--

darcsFormatDir :: Tree m -> Maybe BLC.ByteString
darcsFormatDir t = BLC.fromChunks . concat <$>
                       mapM string (sortBy cmp $ listImmediate t)
    where cmp (a, _) (b, _) = compare a b
          string (name, item) =
              do header <- case item of
                             File _ -> Just $ BC.pack "file:\n"
                             _ -> Just $ BC.pack "directory:\n"
                 hash <- case itemHash item of
                           NoHash -> Nothing
                           x -> Just $ encodeBase16 x
                 return   [ header
                          , encodeWhiteName name
                          , BC.singleton '\n'
                          , hash, BC.singleton '\n' ]

darcsParseDir :: BLC.ByteString -> [(ItemType, Name, Maybe Int, Hash)]
darcsParseDir content = parse (BLC.split '\n' content)
    where
      parse (t:n:h':r) = (header t,
                          decodeWhiteName $ B.concat $ BL.toChunks n,
                          decodeDarcsSize hash,
                          decodeDarcsHash hash) : parse r
          where hash = BC.concat $ BLC.toChunks h'
      parse _ = []
      header x
          | x == BLC.pack "file:" = BlobType
          | x == BLC.pack "directory:" = TreeType
          | otherwise = error $ "Error parsing darcs hashed dir: " ++ BLC.unpack x

----------------------------------------
-- Utilities.
--

-- | Compute a darcs-compatible hash value for a tree-like structure.
darcsTreeHash :: Tree m -> Hash
darcsTreeHash t = case darcsFormatDir t of
                    Nothing -> NoHash
                    Just x -> sha256 x

-- The following two are mostly for experimental use in Packed.

darcsUpdateDirHashes :: Tree m -> Tree m
darcsUpdateDirHashes = updateSubtrees update
    where update t = t { treeHash = darcsTreeHash t }

darcsUpdateHashes :: (Monad m) => Tree m -> m (Tree m)
darcsUpdateHashes = updateTree update
    where update (SubTree t) = return . SubTree $ t { treeHash = darcsTreeHash t }
          update (File blob@(Blob con _)) =
              do hash <- sha256 <$> readBlob blob
                 return $ File (Blob con hash)
          update stub = return stub

darcsHash :: (Monad m) => TreeItem m -> m Hash
darcsHash (SubTree t) = return $ darcsTreeHash t
darcsHash (File blob) = sha256 <$> readBlob blob
darcsHash _ = return NoHash

darcsAddMissingHashes :: (Monad m) => Tree m -> m (Tree m)
darcsAddMissingHashes = addMissingHashes darcsHash

-------------------------------------------
-- Reading darcs pristine data
--

-- | Read and parse a darcs-style hashed directory listing from a given @dir@
-- and with a given @hash@.
readDarcsHashedDir :: FilePath -> (Maybe Int, Hash) -> IO [(ItemType, Name, Maybe Int, Hash)]
readDarcsHashedDir dir h = do
  debugMessage $ "readDarcsHashedDir: " ++ dir ++ " " ++ showHash (snd h)
  exist <- doesFileExist $ fst (darcsLocation dir h)
  unless exist $ fail $ "error opening " ++ fst (darcsLocation dir h)
  compressed <- readSegment $ darcsLocation dir h
  let content = decompress compressed
  return $ if BLC.null compressed
              then []
              else darcsParseDir content

-- | Read in a darcs-style hashed tree. This is mainly useful for reading
-- \"pristine.hashed\". You need to provide the root hash you are interested in
-- (found in _darcs/hashed_inventory).
readDarcsHashed' :: Bool -> FilePath -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed' _ _ (_, NoHash) = fail "Cannot readDarcsHashed NoHash"
readDarcsHashed' sizefail dir root@(_, hash) = do
  items' <- readDarcsHashedDir dir root
  subs <- sequence [
           do when (sizefail && isJust s) $
                fail ("Unexpectedly encountered size-prefixed hash in " ++ dir)
              case tp of
                BlobType -> return (d, File $
                                       Blob (readBlob' (s, h)) h)
                TreeType ->
                  do let t = readDarcsHashed dir (s, h)
                     return (d, Stub t h)
           | (tp, d, s, h) <- items' ]
  return $ makeTreeWithHash subs hash
    where readBlob' = fmap decompress . readSegment . darcsLocation dir

readDarcsHashed :: FilePath -> (Maybe Int, Hash) -> IO (Tree IO)
readDarcsHashed = readDarcsHashed' False

readDarcsHashedNosize :: FilePath -> Hash -> IO (Tree IO)
readDarcsHashedNosize dir hash = readDarcsHashed' True dir (Nothing, hash)

----------------------------------------------------
-- Writing darcs-style hashed trees.
--

-- | Write a Tree into a darcs-style hashed directory.
writeDarcsHashed :: Tree IO -> FilePath -> IO Hash
writeDarcsHashed tree' dir =
    do debugMessage $ "writeDarcsHashed " ++ dir
       t <- darcsUpdateDirHashes <$> expand tree'
       sequence_ [ dump =<< readBlob b | (_, File b) <- list t ]
       let dirs = darcsFormatDir t : [ darcsFormatDir d | (_, SubTree d) <- list t ]
       _ <- mapM (dump . fromJust) dirs
       return $ darcsTreeHash t
    where dump bits =
              do let name = dir </> BC.unpack (encodeBase16 $ sha256 bits)
                 exist <- doesFileExist name
                 unless exist $ BL.writeFile name (compress bits)

-- | Create a hashed file from a 'FilePath' and content. In case the file exists
-- it is kept untouched and is assumed to have the right content. XXX Corrupt
-- files should be probably renamed out of the way automatically or something
-- (probably when they are being read though).
fsCreateHashedFile :: FilePath -> BLC.ByteString -> TreeIO ()
fsCreateHashedFile fn content =
    liftIO $ do
      debugMessage $ "fsCreateHashedFile " ++ fn
      exist <- doesFileExist fn
      unless exist $ BL.writeFile fn content

-- | Run a 'TreeIO' @action@ in a hashed setting. The @initial@ tree is assumed
-- to be fully available from the @directory@, and any changes will be written
-- out to same. Please note that actual filesystem files are never removed.
hashedTreeIO :: TreeIO a -- ^ action
             -> Tree IO -- ^ initial
             -> FilePath -- ^ directory
             -> IO (a, Tree IO)
hashedTreeIO action t dir =
    runTreeMonad action $ initialState t darcsHash updateItem
    where updateItem _ (File b) = File <$> updateFile b
          updateItem _ (SubTree s) = SubTree <$> updateSub s
          updateItem _ x = return x

          updateFile b@(Blob _ !h) = do
            liftIO $ debugMessage $ "hashedTreeIO.updateFile: " ++ showHash h
            content <- liftIO $ readBlob b
            let fn = dir </> showHash h
                nblob = Blob (decompress <$> rblob) h
                rblob = BL.fromChunks . return <$> B.readFile fn
                newcontent = compress content
            fsCreateHashedFile fn newcontent
            return nblob
          updateSub s = do
            let !hash = treeHash s
                Just dirdata = darcsFormatDir s
                fn = dir </> showHash hash
            liftIO $ debugMessage $ "hashedTreeIO.updateSub: " ++ showHash hash
            fsCreateHashedFile fn (compress dirdata)
            return s

showHash :: Hash -> String
showHash = BC.unpack . encodeBase16
