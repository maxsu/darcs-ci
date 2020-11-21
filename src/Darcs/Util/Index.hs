--  Copyright (C) 2009-2011 Petr Rockai
--            (C) 2013 Jose Neder
--  BSD3
{-# LANGUAGE CPP, MultiParamTypeClasses #-}

-- | This module contains plain tree indexing code. The index itself is a
-- CACHE: you should only ever use it as an optimisation and never as a primary
-- storage. In practice, this means that when we change index format, the
-- application is expected to throw the old index away and build a fresh
-- index. Please note that tracking index validity is out of scope for this
-- library: this is responsibility of your application. It is advisable that in
-- your validity tracking code, you also check for format validity (see
-- 'indexFormatValid') and scrap and re-create index when needed.
--
-- The index is a binary file that overlays a hashed tree over the working
-- copy. This means that every working file and directory has an entry in the
-- index, that contains its path and hash and validity data. The validity data
-- is a timestamp plus the file size. The file hashes are sha256's of the
-- file's content. It also contains the fileid to track moved files.
--
-- There are two entry types, a file entry and a directory entry. Both have a
-- common binary format (see 'Item'). The on-disk format is best described by
-- the section /Index format/ below.
--
-- For each file, the index has a copy of the file's last modification
-- timestamp taken at the instant when the hash has been computed. This means
-- that when file size and timestamp of a file in working tree matches those in
-- the index, we assume that the hash stored in the index for given file is
-- valid. These hashes are then exposed in the resulting 'Tree' object, and can
-- be leveraged by eg.  'diffTrees' to compare many files quickly.
--
-- You may have noticed that we also keep hashes of directories. These are
-- assumed to be valid whenever the complete subtree has been valid. At any
-- point, as soon as a size or timestamp mismatch is found, the working file in
-- question is opened, its hash (and timestamp and size) is recomputed and
-- updated in-place in the index file (everything lives at a fixed offset and
-- is fixed size, so this isn't an issue). This is also true of directories:
-- when a file in a directory changes hash, this triggers recomputation of all
-- of its parent directory hashes; moreover this is done efficiently -- each
-- directory is updated at most once during an update run.
--
-- /Endianness/
--
-- Since version 6 (magic == "HSI6"), the file format depends on the endianness
-- of the architecture. To account for the (rare) case where darcs executables
-- from different architectures operate on the same repo, we make an additional
-- check in indexFormatValid to detect whether the file's endianness differs
-- from what we expect. If this is detected, the file is considered invalid and
-- will be re-created.
--
-- /Index format/
--
-- The index starts with a header consisting of a 4 bytes magic word, followed
-- by a 4 byte word to indicate the endianness of the encoding. This word
-- should, when read directly from the mmapped file, be equal to 1. After the
-- header comes the actual content of the index, which is organised into
-- \"lines\" where each line describes a single indexed item. It consists of
--
-- * size: item size, 8 bytes
-- * aux: timestamp (for file) or offset to sibling (for dir), 8 bytes
-- * fileid: inode or fhandle of the item, 8 bytes
-- * hash: sha256 of content, 32 bytes
-- * descriptor length: >= 2 due to type and null, 4 bytes
-- * descriptor:
--   * type: 'D' or 'F', one byte
--   * path: flattened path, variable >= 0
-- * null: terminating null byte
--
-- With directories, the aux holds the offset of the next sibling line in the
-- index, so we can efficiently skip reading the whole subtree starting at a
-- given directory (by just seeking aux bytes forward). The lines are
-- pre-ordered with respect to directory structure -- the directory comes first
-- and after it come all its items. Cf. 'readIndex''.
--
-- For files, the aux field holds a timestamp.
--
-- Internally, the item is stored as a pointer to the first field (iBase)
-- from which we directly read off the first three fields (size, aux, fileid),
-- and a ByteString for the rest (iHashAndDescriptor), up to but not including
-- the terminating null byte.
--
-- Comments by bf:
--
-- The null byte terminator seems useless.
--
-- We could as well use just a single ByteString to represent an item; or even
-- a single raw pointer, since finalizers are needed only when we copy hash and
-- path back to the program as ByteStrings.
--
-- An alternative representation could be to store the fixed-size fields (i.e
-- everything except the path) as an unboxed array of records (structs). The
-- paths would then be stored in a bidirectional map between item indices and
-- paths.

module Darcs.Util.Index
    ( readIndex
    , updateIndexFrom
    , indexFormatValid
    , updateIndex
    , listFileIDs
    , Index
    , filter
    , getFileID
    -- for testing
    , align
    ) where

import Darcs.Prelude hiding ( readFile, writeFile, filter )

import Darcs.Util.ByteString ( readSegment, decodeLocale )
import qualified Darcs.Util.File ( getFileStatus )
import Darcs.Util.Hash( sha256, rawHash )
import Darcs.Util.Tree
import Darcs.Util.Path
    ( AnchoredPath
    , anchorPath
    , anchoredRoot
    , Name
    , rawMakeName
    , appendPath
    , flatten
    )
import Control.Monad( when )
import Control.Exception( catch, throw, SomeException, Exception )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Unsafe( unsafeHead, unsafeDrop )
import Data.ByteString.Internal
    ( c2w
    , fromForeignPtr
    , memcpy
    , nullForeignPtr
    , toForeignPtr
    )

import Data.Int( Int64, Int32 )
import Data.IORef( )
import Data.Maybe( fromJust, isJust, fromMaybe )
import Data.Typeable( Typeable )

import Foreign.Storable
import Foreign.ForeignPtr( ForeignPtr, withForeignPtr, castForeignPtr )
import Foreign.Ptr( Ptr, plusPtr )

import System.IO ( hPutStrLn, stderr )
import System.IO.MMap( mmapFileForeignPtr, Mode(..) )
import System.Directory( doesFileExist, getCurrentDirectory, doesDirectoryExist )
#if mingw32_HOST_OS
import System.Directory( renameFile )
import System.FilePath( (<.>) )
#else
import System.Directory( removeFile )
#endif

#ifdef WIN32
import System.Win32.File
    ( BY_HANDLE_FILE_INFORMATION(..)
    , closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_SHARE_NONE
    , gENERIC_NONE
    , getFileInformationByHandle
    , oPEN_EXISTING
    )
#else
import qualified System.Posix.Files as F ( getSymbolicLinkStatus, fileID )
#endif

import System.FilePath ( (</>) )
import qualified System.Posix.Files as F
    ( modificationTime, fileSize, isDirectory, isSymbolicLink
    , FileStatus
    )
import System.Posix.Types ( FileID, EpochTime, FileOffset )

--------------------------
-- Indexed trees
--

-- | Description of a a single indexed item. The structure itself does not
-- contain any data, just pointers to the underlying mmap (bytestring is a
-- pointer + offset + length).
--
-- The structure is recursive-ish (as opposed to flat-ish structure, which is
-- used by git...) It turns out that it's hard to efficiently read a flat index
-- with our internal data structures -- we need to turn the flat index into a
-- recursive Tree object, which is rather expensive... As a bonus, we can also
-- efficiently implement subtree queries this way (cf. 'readIndex').
data Item = Item { iBase :: !(Ptr ())
                 , iHashAndDescriptor :: !B.ByteString
                 } deriving Show

index_version :: B.ByteString
index_version = BC.pack "HSI6"

-- | Stored to the index to verify we are on the same endianness when reading
-- it back. We will treat the index as invalid in this case so user code will
-- regenerate it.
index_endianness_indicator :: Int32
index_endianness_indicator = 1

size_header, size_magic, size_endianness_indicator :: Int
size_magic = 4 -- the magic word, first 4 bytes of the index
size_endianness_indicator = 4 -- second 4 bytes of the index
size_header = size_magic + size_endianness_indicator

size_dsclen, size_hash, size_size, size_aux, size_fileid :: Int
size_size = 8 -- file/directory size (Int64)
size_aux = 8 -- aux (Int64)
size_fileid = 8 -- fileid (inode or fhandle FileID)
size_dsclen = 4 -- this many bytes store the length of the path
size_hash = 32 -- hash representation
size_type, size_null :: Int
size_type = 1 -- ItemType: 'D' for directory, 'F' for file
size_null = 1 -- null byte at the end of path

off_size, off_aux, off_hash, off_dsc, off_dsclen, off_fileid :: Int
off_size = 0
off_aux = off_size + size_size
off_fileid = off_aux + size_aux
off_dsclen = off_fileid + size_fileid
off_hash = off_dsclen + size_dsclen
off_dsc = off_hash + size_hash

itemAllocSize :: AnchoredPath -> Int
itemAllocSize apath = align 4 $
  size_size + size_aux + size_fileid + size_dsclen + size_hash +
  size_type + B.length (flatten apath) + size_null

itemSize, itemNext :: Item -> Int
itemSize i =
  size_size + size_aux + size_fileid + size_dsclen +
  (B.length $ iHashAndDescriptor i)
-- The "+ 1" is for the null byte at the end, which is /not/
-- contained in iDescriptor!
itemNext i = align 4 (itemSize i + 1)

-- iDescriptor is:
--  * one byte for type of item ('D' or 'F')
--  * flattened path
iHash, iDescriptor :: Item -> B.ByteString
iDescriptor = unsafeDrop size_hash . iHashAndDescriptor
iHash = B.take size_hash . iHashAndDescriptor

-- The "drop 1" here gets rid of the item type.
iPath :: Item -> FilePath
iPath = decodeLocale . unsafeDrop 1 . iDescriptor

iSize, iAux :: Item -> Ptr Int64
iSize i = plusPtr (iBase i) off_size
iAux i = plusPtr (iBase i) off_aux

iFileID :: Item -> Ptr FileID
iFileID i = plusPtr (iBase i) off_fileid

itemIsDir :: Item -> Bool
itemIsDir i = unsafeHead (iDescriptor i) == c2w 'D'

type FileStatus = Maybe F.FileStatus

-- TODO: upgrade to modificationTimeHiRes for nanosecond resolution
modificationTime :: FileStatus -> EpochTime
modificationTime = maybe 0 F.modificationTime

fileSize :: FileStatus -> FileOffset
fileSize = maybe 0 F.fileSize

fileExists :: FileStatus -> Bool
fileExists = maybe False (const True)

isDirectory :: FileStatus -> Bool
isDirectory = maybe False F.isDirectory

-- | Lay out the basic index item structure in memory. The memory location is
-- given by a ForeignPointer () and an offset. The path and type given are
-- written out, and a corresponding Item is given back. The remaining bits of
-- the item can be filled out using 'update'.
createItem :: ItemType -> AnchoredPath -> ForeignPtr () -> Int -> IO Item
createItem typ apath fp off = do
  let dsc =
        B.concat
          [ BC.singleton $ if typ == TreeType then 'D' else 'F'
          , flatten apath -- this (currently) gives "." for anchoredRoot
          , B.singleton 0
          ]
      (dsc_fp, dsc_start, dsc_len) = toForeignPtr dsc
  withForeignPtr fp $ \p ->
    withForeignPtr dsc_fp $ \dsc_p -> do
      fileid <- fromMaybe 0 <$> getFileID apath
      pokeByteOff p (off + off_fileid) (fromIntegral fileid :: Int64)
      pokeByteOff p (off + off_dsclen) (fromIntegral dsc_len :: Int32)
      memcpy
        (plusPtr p $ off + off_dsc)
        (plusPtr dsc_p dsc_start)
        (fromIntegral dsc_len)
      peekItem fp off

-- | Read the on-disk representation into internal data structure.
--
-- See the module-level section /Index format/ for details on how the index
-- is structured.
peekItem :: ForeignPtr () -> Int -> IO Item
peekItem fp off =
  withForeignPtr fp $ \p -> do
    nl' :: Int32 <- peekByteOff p (off + off_dsclen)
    when (nl' <= 2) $ fail "Descriptor too short in peekItem!"
    let nl = fromIntegral nl'
        dsc =
          fromForeignPtr
            (castForeignPtr fp)
            (off + off_hash)
            -- The "- 1" here means we do /not/ include the null byte!
            -- This is why we have to add 1 when we determine the
            -- size, see 'itemSize' and 'itemNext' above.
            (size_hash + nl - 1)
    return $! Item {iBase = plusPtr p off, iHashAndDescriptor = dsc}

-- | Update an existing item with new hash and optionally mtime (give Nothing
-- when updating directory entries).
updateItem :: Item -> Int64 -> Hash -> IO ()
updateItem item _ NoHash =
    fail $ "Index.update NoHash: " ++ iPath item
updateItem item size hash =
    do poke (iSize item) size
       unsafePokeBS (iHash item) (rawHash hash)

updateFileID :: Item -> FileID -> IO ()
updateFileID item fileid = poke (iFileID item) $ fromIntegral fileid
updateAux :: Item -> Int64 -> IO ()
updateAux item aux = poke (iAux item) $ aux
updateTime :: forall a.(Enum a) => Item -> a -> IO ()
updateTime item mtime = updateAux item (fromIntegral $ fromEnum mtime)

iHash' :: Item -> Hash
iHash' i = SHA256 (iHash i)

-- | Gives a ForeignPtr to mmapped index, which can be used for reading and
-- updates. The req_size parameter, if non-0, expresses the requested size of
-- the index file. mmapIndex will grow the index if it is smaller than this.
mmapIndex :: forall a. FilePath -> Int -> IO (ForeignPtr a, Int)
mmapIndex indexpath req_size = do
  act_size <- fromIntegral . fileSize <$> Darcs.Util.File.getFileStatus indexpath
  let size = case req_size > 0 of
        True -> req_size
        False | act_size >= size_header -> act_size - size_header
              | otherwise -> 0
  case size of
    0 -> return (castForeignPtr nullForeignPtr, size)
    _ -> do (x, _, _) <- mmapFileForeignPtr indexpath
                                            ReadWriteEx (Just (0, size + size_header))
            return (x, size)

data IndexM m = Index { mmap :: (ForeignPtr ())
                      , basedir :: FilePath
                      , hashtree :: Tree m -> Hash
                      , predicate :: AnchoredPath -> TreeItem m -> Bool }
              | EmptyIndex

type Index = IndexM IO

-- FIXME This is not really a state: we modify it only when we recurse
-- down into a dir item, so this is rather more like an environment.
-- Instead of passing it explicitly we could use ReaderT.

-- | When we traverse the index, we keep track of some data about the
-- current parent directory.
data State = State
  { dirlength :: !Int     -- ^ length in bytes of current path prefix,
                          --   includes the trailing path separator
  , path :: !AnchoredPath -- ^ path of the current directory
  , start :: !Int         -- ^ offset of current directory in the index
  }

-- * Reading items from the index

data Result = Result
  { changed :: !Bool
  -- ^ Whether item has changed since the last update to the index.
  , next :: !Int
  -- ^ Position of the next item, in bytes.
  , treeitem :: !(Maybe (TreeItem IO))
  -- ^ Nothing in case of the item doesn't exist in the tree
  -- or is filtered by a FilterTree. Or a TreeItem otherwise.
  , resitem :: !Item
  -- ^ The item extracted.
  }

readItem :: Index -> State -> IO Result
readItem index state = do
  item <- peekItem (mmap index) (start state)
  res' <- if itemIsDir item
              then readDir  index state item
              else readFile index state item
  return res'

data CorruptIndex = CorruptIndex String deriving (Eq, Typeable)
instance Exception CorruptIndex
instance Show CorruptIndex where show (CorruptIndex s) = s

-- | Get the 'Name' of an 'Item' in the given 'State'. This fails for
-- the root 'Item' because it has no 'Name', so we return 'Nothing'.
nameof :: Item -> State -> Maybe Name
nameof item state
  | iDescriptor item == BC.pack "D." = Nothing
  | otherwise =
      case rawMakeName $ B.drop (dirlength state + 1) $ iDescriptor item of
        Left msg -> throw (CorruptIndex msg)
        Right name -> Just name

-- | 'Maybe' append a 'Name' to an 'AnchoredPath'.
maybeAppendName :: AnchoredPath -> Maybe Name -> AnchoredPath
maybeAppendName parent = maybe parent (parent `appendPath`)

-- | Calculate the next 'State' when entering an 'Item'. Works for the
-- top-level 'Item' i.e. the root directory only because we handle that
-- specially.
substateof :: Item -> State -> State
substateof item state =
  state
    { start = start state + itemNext item
    , path = path state `maybeAppendName` myname
    , dirlength =
        case myname of
          Nothing ->
            -- We are entering the root item. The current path prefix remains
            -- empty, so its length (which must be 0) doesn't change.
            dirlength state
          Just _ ->
            -- This works because the 'iDescriptor' is always one byte larger
            -- than the actual name. So @dirlength state@ will also be greater
            -- by 1, which accounts for the path separator when we strip the
            -- directory prefix from the full path.
            B.length (iDescriptor item)
    }
  where
    myname = nameof item state

readDir :: Index -> State -> Item -> IO Result
readDir index state item = do
       following <- fromIntegral <$> peek (iAux item)
       st <- getFileStatus (iPath item)
       let exists = fileExists st && isDirectory st
       fileid <- fromIntegral <$> (peek $ iFileID item)
       fileid' <- fromMaybe fileid <$> (getFileID' $ iPath item)
       when (fileid == 0) $ updateFileID item fileid'
       let substate = substateof item state

           want = exists && (predicate index) (path substate) (Stub undefined NoHash)
           oldhash = iHash' item

           subs off =
              case compare off following of
                LT -> do
                  result <- readItem index $ substate { start = off }
                  rest <- subs $ next result
                  return $! (nameof (resitem result) substate, result) : rest
                EQ -> return []
                GT ->
                  fail $
                    "Offset mismatch at " ++ show off ++
                    " (ends at " ++ show following ++ ")"

       inferiors <- if want then subs $ start substate
                            else return []

       let we_changed = or [ changed x | (_, x) <- inferiors ] || nullleaf
           nullleaf = null inferiors && oldhash == nullsha
           nullsha = SHA256 (B.replicate 32 0)
           tree' =
             -- Note the partial pattern match on 'Just n' below is justified
             -- as we are traversing sub items here, which means 'Nothing' is
             -- impossible, see 'substateof' for details.
             makeTree
               [ (n, fromJust $ treeitem s)
               | (Just n, s) <- inferiors, isJust $ treeitem s ]
           treehash = if we_changed then hashtree index tree' else oldhash
           tree = tree' { treeHash = treehash }

       when (exists && we_changed) $ updateItem item 0 treehash
       return $ Result { changed = not exists || we_changed
                       , next = following
                       , treeitem = if want then Just $ SubTree tree
                                            else Nothing
                       , resitem = item }

readFile :: Index -> State -> Item -> IO Result
readFile index state item = do
       st <- getFileStatus (iPath item)
       mtime <- fromIntegral <$> (peek $ iAux item)
       size <- peek $ iSize item
       fileid <- fromIntegral <$> (peek $ iFileID item)
       fileid' <- fromMaybe fileid <$> (getFileID' $ iPath item)
       let mtime' = modificationTime st
           size' = fromIntegral $ fileSize st
           readblob = readSegment (basedir index </> (iPath item), Nothing)
           exists = fileExists st && not (isDirectory st)
           we_changed = mtime /= mtime' || size /= size'
           hash = iHash' item
       when (exists && we_changed) $
            do hash' <- sha256 `fmap` readblob
               updateItem item size' hash'
               updateTime item mtime'
               when (fileid == 0) $ updateFileID item fileid'
       return $ Result { changed = not exists || we_changed
                       , next = start state + itemNext item
                       , treeitem = if exists then Just $ File $ Blob readblob hash else Nothing
                       , resitem = item }

-- * Reading (only) file IDs from the index

-- FIXME this seems copy-pasted from the code above and then adapted
-- to the purpose. Should factor out the traversal of the index as a
-- higher order function.

data ResultF = ResultF
  { nextF :: !Int
  -- ^ Position of the next item, in bytes.
  , resitemF :: !Item
  -- ^ The item extracted.
  , _fileIDs :: [((AnchoredPath, ItemType), FileID)]
  -- ^ The fileids of the files and folders inside,
  -- in a folder item and its own fileid for file item).
  }

-- | Return a list containing all the file/folder names in an index, with
-- their respective ItemType and FileID.
listFileIDs :: Index -> IO ([((AnchoredPath, ItemType), FileID)])
listFileIDs EmptyIndex = return []
listFileIDs index =
    do let initial = State { start = size_header
                           , dirlength = 0
                           , path = anchoredRoot }
       res <- readItemFileIDs index initial
       return $ _fileIDs res

readItemFileIDs :: Index -> State -> IO ResultF
readItemFileIDs index state = do
  item <- peekItem (mmap index) (start state)
  res' <- if itemIsDir item
              then readDirFileIDs  index state item
              else readFileFileID index state item
  return res'

readDirFileIDs :: Index -> State -> Item -> IO ResultF
readDirFileIDs index state item =
    do fileid <- fromIntegral <$> (peek $ iFileID item)
       following <- fromIntegral <$> peek (iAux item)
       let substate = substateof item state
           subs off =
              case compare off following of
                LT -> do
                  result <- readItemFileIDs index $ substate {start = off}
                  rest <- subs $ nextF result
                  return $! (nameof (resitemF result) substate, result) : rest
                EQ -> return []
                GT ->
                  fail $
                    "Offset mismatch at " ++ show off ++
                    " (ends at " ++ show following ++ ")"
       inferiors <- subs $ start substate
       return $ ResultF { nextF = following
                        , resitemF = item
                        , _fileIDs = (((path substate, TreeType), fileid):concatMap (_fileIDs . snd) inferiors) }

readFileFileID :: Index -> State -> Item -> IO ResultF
readFileFileID _ state item =
    do fileid' <- fromIntegral <$> (peek $ iFileID item)
       let myname = nameof item state
       return $ ResultF { nextF = start state + itemNext item
                        , resitemF = item
                        , _fileIDs = [((path state `maybeAppendName` myname, BlobType), fileid')] }

-- * Reading and writing 'Tree's from/to the index

-- | Read an index and build up a 'Tree' object from it, referring to current
-- working directory. The initial Index object returned by readIndex is not
-- directly useful. However, you can use 'Tree.filter' on it. Either way, to
-- obtain the actual Tree object, call update.
--
-- The usual use pattern is this:
--
-- > do (idx, update) <- readIndex
-- >    tree <- update =<< filter predicate idx
--
-- The resulting tree will be fully expanded.
readIndex :: FilePath -> (Tree IO -> Hash) -> IO Index
readIndex indexpath ht = do
  (mmap_ptr, mmap_size) <- mmapIndex indexpath 0
  base <- getCurrentDirectory
  return $ if mmap_size == 0 then EmptyIndex
                             else Index { mmap = mmap_ptr
                                        , basedir = base
                                        , hashtree = ht
                                        , predicate = \_ _ -> True }

formatIndex :: ForeignPtr () -> Tree IO -> Tree IO -> IO ()
formatIndex mmap_ptr old reference =
    do _ <- create (SubTree reference) (anchoredRoot) size_header
       unsafePokeBS magic index_version
       withForeignPtr mmap_ptr $ \ptr ->
         pokeByteOff ptr size_magic index_endianness_indicator
    where magic = fromForeignPtr (castForeignPtr mmap_ptr) 0 4
          create (File _) path' off =
               do i <- createItem BlobType path' mmap_ptr off
                  let flatpath = anchorPath "" path'
                  case find old path' of
                    Nothing -> return ()
                    -- TODO calling getFileStatus here is both slightly
                    -- inefficient and slightly race-prone
                    Just ti -> do st <- getFileStatus flatpath
                                  let hash = itemHash ti
                                      mtime = modificationTime st
                                      size = fileSize st
                                  updateItem i (fromIntegral size) hash
                                  updateTime i mtime
                  return $ off + itemNext i
          create (SubTree s) path' off =
               do i <- createItem TreeType path' mmap_ptr off
                  case find old path' of
                    Nothing -> return ()
                    Just ti | itemHash ti == NoHash -> return ()
                            | otherwise -> updateItem i 0 $ itemHash ti
                  let subs [] = return $ off + itemNext i
                      subs ((name,x):xs) = do
                        let path'' = path' `appendPath` name
                        noff <- subs xs
                        create x path'' noff
                  lastOff <- subs (listImmediate s)
                  poke (iAux i) (fromIntegral lastOff)
                  return lastOff
          create (Stub _ _) path' _ =
               fail $ "Cannot create index from stubbed Tree at " ++ show path'

-- | Will add and remove files in index to make it match the 'Tree' object
-- given (it is an error for the 'Tree' to contain a file or directory that
-- does not exist in a plain form in current working directory).
updateIndexFrom :: FilePath -> (Tree IO -> Hash) -> Tree IO -> IO Index
updateIndexFrom indexpath hashtree' ref =
    do old_idx <- updateIndex =<< readIndex indexpath hashtree'
       reference <- expand ref
       let len_root = itemAllocSize anchoredRoot
           len = len_root + sum [ itemAllocSize p | (p, _) <- list reference ]
       exist <- doesFileExist indexpath
-- TODO this conditional logic (rename or delete) is mirrored in
-- Darcs.Repository.State.checkIndex and should be refactored
#if mingw32_HOST_OS
       when exist $ renameFile indexpath (indexpath <.> "old")
#else
       when exist $ removeFile indexpath -- to avoid clobbering oldidx
#endif
       (mmap_ptr, _) <- mmapIndex indexpath len
       formatIndex mmap_ptr old_idx reference
       readIndex indexpath hashtree'

updateIndex :: Index -> IO (Tree IO)
updateIndex EmptyIndex = return emptyTree
updateIndex index =
    do let initial = State { start = size_header
                           , dirlength = 0
                           , path = anchoredRoot }
       res <- readItem index initial
       case treeitem res of
         Just (SubTree tree) -> return $ filter (predicate index) tree
         _ -> fail "Unexpected failure in updateIndex!"

-- | Check that a given file is an index file with a format we can handle. You
-- should remove and re-create the index whenever this is not true.
indexFormatValid :: FilePath -> IO Bool
indexFormatValid path' =
  do
    (start, _, _) <- mmapFileForeignPtr path' ReadOnly (Just (0, size_header))
    let magic = fromForeignPtr (castForeignPtr start) 0 4
    endianness_indicator <- withForeignPtr start $ \ptr -> peekByteOff ptr 4
    return $
      index_version == magic && index_endianness_indicator == endianness_indicator
  `catch` \(_::SomeException) -> return False

instance FilterTree IndexM IO where
    filter _ EmptyIndex = EmptyIndex
    filter p index = index { predicate = \a b -> predicate index a b && p a b }


-- * Getting the file ID from a path

-- | For a given file or folder path, get the corresponding fileID from the
-- filesystem.
getFileID :: AnchoredPath -> IO (Maybe FileID)
getFileID = getFileID' . anchorPath ""

getFileID' :: FilePath -> IO (Maybe FileID)
getFileID' fp = do
  file_exists <- doesFileExist fp
  dir_exists <- doesDirectoryExist fp
  if file_exists || dir_exists
#ifdef WIN32
    then do
      h <-
        createFile fp gENERIC_NONE fILE_SHARE_NONE Nothing
        oPEN_EXISTING fILE_FLAG_BACKUP_SEMANTICS Nothing
      fhnumber <-
        (Just . fromIntegral . bhfiFileIndex) <$> getFileInformationByHandle h
      closeHandle h
      return fhnumber
#else
    then (Just . F.fileID) <$> F.getSymbolicLinkStatus fp
#endif
    else return Nothing


-- * Low-level utilities

-- Wow, unsafe.
unsafePokeBS :: BC.ByteString -> BC.ByteString -> IO ()
unsafePokeBS to from =
    do let (fp_to, off_to, len_to) = toForeignPtr to
           (fp_from, off_from, len_from) = toForeignPtr from
       when (len_to /= len_from) $ fail $ "Length mismatch in unsafePokeBS: from = "
            ++ show len_from ++ " /= to = " ++ show len_to
       withForeignPtr fp_from $ \p_from ->
         withForeignPtr fp_to $ \p_to ->
           memcpy (plusPtr p_to off_to)
                  (plusPtr p_from off_from)
                  (fromIntegral len_to)

align :: Integral a => a -> a -> a
align boundary i = case i `rem` boundary of
                     0 -> i
                     x -> i + boundary - x
{-# INLINE align #-}

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  mst <- Darcs.Util.File.getFileStatus path
  case mst of
    Just st
      | F.isSymbolicLink st -> do
          hPutStrLn stderr $ "Warning: ignoring symbolic link " ++ path
          return Nothing
    _ -> return mst
