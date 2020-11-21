-- Copyright (C) 2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Darcs.Repository.HashedIO ( copyHashed, copyPartialsHashed,
                                   cleanHashdir, getHashedFiles,
                                   pathsAndContents
                                 ) where

import Darcs.Prelude

import Darcs.Util.Global ( darcsdir )
import qualified Data.Set as Set
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Monad.State ( StateT, runStateT, modify, get, put, gets, lift, evalStateT )
import Control.Monad ( when, void, unless, guard )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Repository.Cache ( Cache, fetchFileUsingCache, writeFileUsingCache,
                                peekInCache, speculateFileUsingCache,
                                okayHash, cleanCachesWithHint, HashedDir(..), hashedDir )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..), ApplyMonadTree(..) )
import Darcs.Repository.Flags ( Compression( .. ), WithWorkingDir (..) )
import Darcs.Repository.Inventory ( PristineHash, getValidHash, mkValidHash )
import Darcs.Util.Lock ( writeAtomicFilePS, removeFileMayNotExist )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Progress ( debugMessage, tediousSize, finishedOneIO )
import Darcs.Util.Path
    ( AnchoredPath
    , anchorPath
    , anchoredRoot
    , parent
    , breakOnDir
    , Name
    , name2fp
    , decodeWhiteName
    , encodeWhiteName
    , isMaliciousSubPath
    )

import Darcs.Util.ByteString ( linesPS, unlinesPS )
import qualified Data.ByteString       as B  (ByteString, length, empty)
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Darcs.Util.Tree.Hashed( readDarcsHashedDir, darcsLocation,
                             decodeDarcsHash, decodeDarcsSize )
import Darcs.Util.Tree( ItemType(..), Tree )

ap2fp :: AnchoredPath -> FilePath
ap2fp = anchorPath ""


-- | @readHashFile c subdir hash@ reads the file with hash @hash@ in dir subdir,
-- fetching it from 'Cache' @c@ if needed. The return value is a pair of the
-- absolute file path and the content.
readHashFile :: Cache -> HashedDir -> PristineHash -> IO (FilePath,B.ByteString)
readHashFile c subdir hash =
    do debugMessage $ "Reading hash file "++getValidHash hash++" from "++hashedDir subdir++"/"
       r <- fetchFileUsingCache c subdir (getValidHash hash)
       debugMessage $ "Result of reading hash file: " ++ show r
       return r

-- TODO an obvious optimization would be to remember
-- the current path and a stack of directories we opened.
-- Then we could batch operations in the same directory and write the
-- result back only when we pop a dir off teh stack.
data HashDir = HashDir { cache :: !Cache,
                         cwdHash :: !PristineHash }
type HashedIO = StateT HashDir IO

mWithSubDirectory :: Name -> HashedIO a -> HashedIO a
mWithSubDirectory dir j = do
  cwd <- readcwd
  case geta D dir cwd of
    Nothing -> fail "dir doesn't exist in mWithSubDirectory..."
    Just h -> do
      (h', x) <- withh h j
      -- update the parent object with new entry
      writecwd $ seta D dir h' cwd
      return x

-- | This is withCurrentDirectory for read-only actions.
mInSubDirectory :: Name -> HashedIO a -> HashedIO a
mInSubDirectory dir j = do
  cwd <- readcwd
  case geta D dir cwd of
    Nothing -> fail "dir doesn't exist..."
    Just h -> inh h j

instance ApplyMonad Tree HashedIO where
    type ApplyMonadBase HashedIO = IO

instance ApplyMonadTree HashedIO where
    mDoesDirectoryExist path = do
      thing <- identifyThing path
      case thing of
        Just (D, _) -> return True
        _ -> return False

    mReadFilePS = readFileObject

    mCreateDirectory path = do
      h <- writeHashFile B.empty
      exists <- isJust `fmap` identifyThing path
      when exists $ fail "can't mCreateDirectory over an existing object."
      addThing path (D, h)

    mRename o n = do
      nexists <- isJust `fmap` identifyThing n
      when nexists $ fail "mRename failed..."
      mx <- identifyThing o
                     -- for backwards compatibility accept rename of nonexistent files.
      case mx of
        Nothing -> return ()
        Just x -> do
          rmThing o
          addThing n x

    mRemoveDirectory = rmThing

    mRemoveFile f = do
      x <- mReadFilePS f
      when (B.length x /= 0) $ fail $ "Cannot remove non-empty file " ++ ap2fp f
      rmThing f

readFileObject :: AnchoredPath -> HashedIO B.ByteString
readFileObject path
  | path == anchoredRoot = fail "root dir is not a file..."
  | otherwise =
      case breakOnDir path of
        Left file -> do
          cwd <- readcwd
          case geta F file cwd of
                Nothing -> fail $ "file doesn't exist..." ++ ap2fp path
                Just h -> readhash h
        Right (name, path') -> do
          mInSubDirectory name $ readFileObject path'

identifyThing :: AnchoredPath -> HashedIO (Maybe (ObjType,PristineHash))
identifyThing path
  | path == anchoredRoot = do
      h <- gets cwdHash
      return $ Just (D, h)
  | otherwise =
      case breakOnDir path of
        Left name -> getany name `fmap` readcwd
        Right (dir, path') -> do
          cwd <- readcwd
          case geta D dir cwd of
            Nothing -> return Nothing
            Just h -> inh h $ identifyThing path'

addThing :: AnchoredPath -> (ObjType,PristineHash) -> HashedIO ()
addThing path (o, h) =
  case breakOnDir path of
    Left name -> seta o name h `fmap` readcwd >>= writecwd
    Right (name,path') -> mWithSubDirectory name $ addThing path' (o,h)

rmThing :: AnchoredPath -> HashedIO ()
rmThing path = 
  case breakOnDir path of
    Left name -> do
      cwd <- readcwd
      let cwd' = filter (\(_,x,_)->x/= name) cwd
      if length cwd' == length cwd - 1
        then writecwd cwd'
        else fail "obj doesn't exist in rmThing"
    Right (name,path') -> mWithSubDirectory name $ rmThing path'

readhash :: PristineHash -> HashedIO B.ByteString
readhash h = do c <- gets cache
                z <- lift $ unsafeInterleaveIO $ readHashFile c HashedPristineDir h
                let (_,out) = z
                return out

withh :: PristineHash -> HashedIO a -> HashedIO (PristineHash,a)
withh h j = do hd <- get
               put $ hd { cwdHash = h }
               x <- j
               h' <- gets cwdHash
               put hd
               return (h',x)

inh :: PristineHash -> HashedIO a -> HashedIO a
inh h j = snd `fmap` withh h j

type DirEntry = (ObjType, Name, PristineHash)

readcwd :: HashedIO [DirEntry]
readcwd = do haveitalready <- peekroot
             cwd <- gets cwdHash >>= readdir
             unless haveitalready $ speculate cwd
             return cwd
    where speculate :: [(a,b,PristineHash)] -> HashedIO ()
          speculate c = do cac <- gets cache
                           mapM_ (\(_,_,z) -> lift $ speculateFileUsingCache cac HashedPristineDir (getValidHash z)) c
          peekroot :: HashedIO Bool
          peekroot = do HashDir c h <- get
                        lift $ peekInCache c HashedPristineDir (getValidHash h)

writecwd :: [DirEntry] -> HashedIO ()
writecwd c = do
  h <- writedir c
  modify $ \hd -> hd { cwdHash = h }

data ObjType = F | D deriving Eq

-- | @geta objtype name direntries@ tries to find an object of type @objtype@ named @name@
-- in @direntries@.
geta :: ObjType -> Name -> [DirEntry] -> Maybe PristineHash
geta o f c = do
  (o', h) <- getany f c
  guard (o == o')
  return h

getany :: Name -> [DirEntry] -> Maybe (ObjType,PristineHash)
getany _ [] = Nothing
getany f ((o,f',h):_) | f == f' = Just (o,h)
getany f (_:r) = getany f r

seta :: ObjType -> Name -> PristineHash -> [DirEntry] -> [DirEntry]
seta o f h [] = [(o,f,h)]
seta o f h ((_,f',_):r) | f == f' = (o,f,h):r
seta o f h (x:xs) = x : seta o f h xs

readdir :: PristineHash -> HashedIO [DirEntry]
readdir hash = do
    content <- readhash hash
    -- lift $ debugMessage  $ show x
    let r = (parseLines . linesPS) content
    --lift $ debugMessage  $ unlines $ map (\(_,path,_) -> "DEBUG readdir " ++
    --  hash ++ " entry: " ++ show path) r
    return r
  where
    parseLines (t:n:h:rest)
      | t == dirType = (D, decodeWhiteName n, mkValidHash $ BC.unpack h) : parseLines rest
      | t == fileType = (F, decodeWhiteName n, mkValidHash $ BC.unpack h) : parseLines rest
    parseLines _ = []

dirType :: B.ByteString
dirType = BC.pack "directory:"

fileType :: B.ByteString
fileType = BC.pack "file:"

writedir :: [DirEntry] -> HashedIO PristineHash
writedir c = do
  --lift $ debugMessage  $ unlines $ map (\(_,path,_) -> "DEBUG writedir entry: " ++ show path) c
  writeHashFile cps
  where
    cps = unlinesPS $ concatMap wr c ++ [B.empty]
    wr (o,d,h) = [showO o, encodeWhiteName d, BC.pack (getValidHash h)]
    showO D = dirType
    showO F = fileType

writeHashFile :: B.ByteString -> HashedIO PristineHash
writeHashFile ps = do
  c <- gets cache
  -- pristine files are always compressed
  lift $ mkValidHash <$> writeFileUsingCache c GzipCompression HashedPristineDir ps

type ProgressKey = String

-- | Grab a whole pristine tree from a hash, and, if asked,
--   write files in the working tree.
copyHashed :: ProgressKey -> Cache -> WithWorkingDir -> PristineHash -> IO ()
copyHashed k c wwd z = void . runStateT cph $ HashDir { cache = c, cwdHash = z }
    where cph = do cwd <- readcwd
                   lift $ tediousSize k (length cwd)
                   mapM_ cp cwd
          cp (F,n,h) = do
              ps <- readhash h
              lift $ finishedOneIO k $ name2fp n
              --lift $ debugMessage $ "DEBUG copyHashed " ++ show n
              case wwd of
                WithWorkingDir -> lift $ writeAtomicFilePS (name2fp n) ps
                NoWorkingDir   -> ps `seq` return ()
                                  -- force evaluation of ps to actually copy hashed file
          cp (D,n,h) =
              if isMaliciousSubPath (name2fp n)
                 then fail ("Caught malicious path: " ++ name2fp n)
                 else do
                 lift $ finishedOneIO k (name2fp n)
                 case wwd of
                   WithWorkingDir -> do
                     lift $ createDirectoryIfMissing False (name2fp n)
                     lift $ withCurrentDirectory (name2fp n) $ copyHashed k c WithWorkingDir h
                   NoWorkingDir ->
                     lift $ copyHashed k c NoWorkingDir h

-- | Returns a list of pairs (FilePath, (strict) ByteString) of
--   the pristine tree starting with the hash @root@.
--   @path@ should be either "." or end with "/"
--   Separator "/" is used since this function is used to generate
--   zip archives from pristine trees.
pathsAndContents :: FilePath -> Cache ->  PristineHash -> IO [(FilePath,B.ByteString)]
pathsAndContents path c root = evalStateT cph HashDir { cache = c, cwdHash = root }
    where cph = do cwd <- readcwd
                   pacs <- concat <$> mapM cp cwd
                   let current = if path == "." then [] else [(path ++ "/" , B.empty)]
                   return $ current ++ pacs
          cp (F,n,h) = do
              ps <- readhash h
              let p = (if path == "." then "" else path ++ "/") ++ name2fp n
              return [(p,ps)]
          cp (D,n,h) = do
              let p = (if path == "." then "" else path) ++ name2fp n ++ "/"
              lift $ pathsAndContents p c h

copyPartialsHashed :: Cache -> PristineHash -> [AnchoredPath] -> IO ()
copyPartialsHashed c root = mapM_ (copyPartialHashed c root)

copyPartialHashed :: Cache -> PristineHash -> AnchoredPath -> IO ()
copyPartialHashed c root path = do
    case parent path of
      Nothing -> return ()
      Just super ->
        createDirectoryIfMissing True (ap2fp super)
    void $ runStateT copy HashDir {cache = c, cwdHash = root}
  where
    copy = do
      mt <- identifyThing path
      case mt of
        Just (D, h) -> do
          lift $ createDirectoryIfMissing True (ap2fp path)
          lift $
            withCurrentDirectory (ap2fp path) $ copyHashed "" c WithWorkingDir h
        Just (F, h) -> do
          ps <- readhash h
          lift $ writeAtomicFilePS (ap2fp path) ps
        Nothing -> return () -- hmm, ignore unknown paths, maybe better fail?

cleanHashdir :: Cache -> HashedDir -> [PristineHash] -> IO ()
cleanHashdir c dir hashroots =
   do -- we'll remove obsolete bits of "dir"
      debugMessage $ "Cleaning out " ++ hashedDir dir ++ "..."
      let hashdir = darcsdir ++ "/" ++ hashedDir dir ++ "/"
      hs <- set <$> getHashedFiles hashdir (map getValidHash hashroots)
      fs <- set . filter okayHash <$> getDirectoryContents hashdir
      mapM_ (removeFileMayNotExist . (hashdir++)) (unset $ fs `Set.difference` hs)
      -- and also clean out any global caches.
      debugMessage "Cleaning out any global caches..."
      cleanCachesWithHint c dir (unset $ fs `Set.difference` hs)
   where set = Set.fromList . map BC.pack
         unset = map BC.unpack . Set.toList

-- | getHashedFiles returns all hash files targeted by files in hashroots in
-- the hashdir directory.
getHashedFiles :: FilePath -> [String] -> IO [String]
getHashedFiles hashdir hashroots = do
  let listone h = do
        let size = decodeDarcsSize $ BC.pack h
            hash = decodeDarcsHash $ BC.pack h
        x <- readDarcsHashedDir hashdir (size, hash)
        let subs = [fst $ darcsLocation "" (s, h') | (TreeType, _, s, h') <- x]
            hashes = h : [fst $ darcsLocation "" (s, h') | (_, _, s, h') <- x]
        (hashes ++) . concat <$> mapM listone subs
  concat <$> mapM listone hashroots
