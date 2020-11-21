{-# LANGUAGE CPP #-}
module Darcs.Util.External
    ( cloneTree
    , cloneFile
    , fetchFilePS
    , fetchFileLazyPS
    , gzFetchFilePS
    , speculateFileOrUrl
    , copyFileOrUrl
    , Cachable(..)
    , backupByRenaming
    , backupByCopying
    ) where

import Control.Exception ( catch, IOException )

import System.Posix.Files
    ( getSymbolicLinkStatus
    , isRegularFile
    , isDirectory
    , createLink
    )
import System.Directory
    ( createDirectory
    , listDirectory
    , doesDirectoryExist
    , doesFileExist
    , renameFile
    , renameDirectory
    , copyFile
    )

import System.FilePath.Posix ( (</>), normalise )
import System.IO.Error ( isDoesNotExistError )
import Control.Monad
    ( unless
    , when
    , zipWithM_
    )

import Darcs.Prelude

import Darcs.Util.Global ( defaultRemoteDarcsCmd )
import Darcs.Util.Download ( Cachable(..) )
#ifdef HAVE_CURL
import Darcs.Util.Download ( copyUrl, copyUrlFirst, waitUrl )
#endif
import qualified Darcs.Util.HTTP as HTTP
import Darcs.Util.URL
    ( isValidLocalPath
    , isHttpUrl
    , isSshUrl
    , splitSshUrl
    )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Lock ( withTemp )
import Darcs.Util.Ssh ( copySSH )
import Darcs.Util.ByteString ( gzReadFilePS )
import qualified Data.ByteString as B (ByteString, readFile )
import qualified Data.ByteString.Lazy as BL

import Network.URI
    ( parseURI
    , uriScheme
    )

copyFileOrUrl :: String    -- ^ remote darcs executable
              -> FilePath  -- ^ path representing the origin file or URL
              -> FilePath  -- ^ destination path
              -> Cachable  -- ^ tell whether file to copy is cachable
              -> IO ()
copyFileOrUrl _    fou out _     | isValidLocalPath fou = copyLocal fou out
copyFileOrUrl _    fou out cache | isHttpUrl  fou = copyRemote fou out cache
copyFileOrUrl rd   fou out _     | isSshUrl  fou = copySSH rd (splitSshUrl fou) out
copyFileOrUrl _    fou _   _     = fail $ "unknown transport protocol: " ++ fou

copyLocal  :: String -> FilePath -> IO ()
copyLocal fou out = createLink fou out `catchall` cloneFile fou out

cloneTree :: FilePath -> FilePath -> IO ()
cloneTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        fps <- listDirectory source
        zipWithM_ cloneSubTree (map (source </>) fps) (map (dest </>) fps)
     else fail ("cloneTree: Bad source " ++ source)
   `catch` \(_ :: IOException) -> fail ("cloneTree: Bad source " ++ source)

cloneSubTree :: FilePath -> FilePath -> IO ()
cloneSubTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        createDirectory dest
        fps <- listDirectory source
        zipWithM_ cloneSubTree (map (source </>) fps) (map (dest </>) fps)
     else if isRegularFile fs then
        cloneFile source dest
     else fail ("cloneSubTree: Bad source "++ source)
    `catch` (\e -> unless (isDoesNotExistError e) $ ioError e)

cloneFile :: FilePath -> FilePath -> IO ()
cloneFile = copyFile

backupByRenaming :: FilePath -> IO ()
backupByRenaming = backupBy rename
 where rename x y = do
         isD <- doesDirectoryExist x
         if isD then renameDirectory x y else renameFile x y

backupByCopying :: FilePath -> IO ()
backupByCopying = backupBy copy
 where
  copy x y = do
    isD <- doesDirectoryExist x
    if isD then do createDirectory y
                   cloneTree (normalise x) (normalise y)
           else copyFile x y

backupBy :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
backupBy backup f =
           do hasBF <- doesFileExist f
              hasBD <- doesDirectoryExist f
              when (hasBF || hasBD) $ helper 0
  where
  helper :: Int -> IO ()
  helper i = do existsF <- doesFileExist next
                existsD <- doesDirectoryExist next
                if existsF || existsD
                   then helper (i + 1)
                   else do putStrLn $ "Backing up " ++ f ++ "(" ++ suffix ++ ")"
                           backup f next
             where next = f ++ suffix
                   suffix = ".~" ++ show i ++ "~"

copyAndReadFile :: (FilePath -> IO a) -> String -> Cachable -> IO a
copyAndReadFile readfn fou _ | isValidLocalPath fou = readfn fou
copyAndReadFile readfn fou cache = withTemp $ \t -> do
  copyFileOrUrl defaultRemoteDarcsCmd fou t cache
  readfn t

-- | @fetchFile fileOrUrl cache@ returns the content of its argument (either a
-- file or an URL). If it has to download an url, then it will use a cache as
-- required by its second argument.
--
-- We always use default remote darcs, since it is not fatal if the remote
-- darcs does not exist or is too old -- anything that supports transfer-mode
-- should do, and if not, we will fall back to SFTP or SCP.
fetchFilePS :: String -> Cachable -> IO B.ByteString
fetchFilePS = copyAndReadFile (B.readFile)

-- | @fetchFileLazyPS fileOrUrl cache@ lazily reads the content of its argument
-- (either a file or an URL). Warning: this function may constitute a fd leak;
-- make sure to force consumption of file contents to avoid that. See
-- "fetchFilePS" for details.
fetchFileLazyPS :: String -> Cachable -> IO BL.ByteString
fetchFileLazyPS x c = case parseURI x of
  Just x' | uriScheme x' == "http:" -> HTTP.copyRemoteLazy x c
  _ -> copyAndReadFile BL.readFile x c

gzFetchFilePS :: String -> Cachable -> IO B.ByteString
gzFetchFilePS = copyAndReadFile gzReadFilePS

speculateFileOrUrl :: String -> FilePath -> IO ()
speculateFileOrUrl fou out | isHttpUrl fou = speculateRemote fou out
                           | otherwise = return ()

copyRemote :: String -> FilePath -> Cachable -> IO ()
speculateRemote :: String -> FilePath -> IO () -- speculations are always Cachable

#ifdef HAVE_CURL
copyRemote u v cache = copyUrlFirst u v cache >> waitUrl u
speculateRemote u v = copyUrl u v Cachable
#else
copyRemote = HTTP.copyRemote
speculateRemote = HTTP.speculateRemote
#endif
