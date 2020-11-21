-- Copyright (C) 2007 Eric Kow
-- Copyright (C) 2010 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE CPP #-}
module Darcs.Util.Path
    ( encodeWhite
    , decodeWhite
    , encodeWhiteName
    , decodeWhiteName
    -- * AbsolutePath
    , AbsolutePath
    , makeAbsolute
    , ioAbsolute
    -- * AbsolutePathOrStd
    , AbsolutePathOrStd
    , makeAbsoluteOrStd
    , ioAbsoluteOrStd
    , useAbsoluteOrStd
    , stdOut
    -- * AbsoluteOrRemotePath
    , AbsoluteOrRemotePath
    , ioAbsoluteOrRemote
    , isRemote
    -- * SubPath
    , SubPath
    , makeSubPathOf
    , simpleSubPath
    , floatSubPath
    -- * Miscellaneous
    , FilePathOrURL(..)
    , FilePathLike(toFilePath)
    , getCurrentDirectory
    , setCurrentDirectory
    , getUniquePathName
    , doesPathExist
    -- * Check for malicious paths
    , isMaliciousSubPath
    -- * Tree filtering.
    , filterPaths
    -- * AnchoredPaths: relative paths within a Tree. All paths are
    -- anchored at a certain root (this is usually the Tree root). They are
    -- represented by a list of Names (these are just strict bytestrings).
    , Name
    , name2fp
    , makeName
    , rawMakeName
    , eqAnycase
    , AnchoredPath(..)
    , anchoredRoot
    , appendPath
    , anchorPath
    , isPrefix
    , breakOnDir
    , movedirfilename
    , parent
    , parents
    , replaceParent
    , catPaths
    , flatten
    , inDarcsdir
    , displayPath
    , realPath
    , isRoot
    , darcsdirName
    -- * Unsafe AnchoredPath functions.
    , floatPath
    ) where

import Darcs.Prelude

import Data.List
    ( isPrefixOf
    , isSuffixOf
    , stripPrefix
    , intersect
    , inits
    )
import Data.Char ( isSpace, chr, ord, toLower )
import Data.Typeable ( Typeable )
import Control.Exception ( tryJust, bracket_, throw, Exception )
import Control.Monad ( when )
import System.IO.Error ( isDoesNotExistError )

import qualified Darcs.Util.Workaround as Workaround ( getCurrentDirectory )
import qualified System.Directory ( setCurrentDirectory )
import System.Directory ( doesDirectoryExist, doesFileExist )
import qualified System.FilePath.Posix as FilePath ( (</>), normalise, isRelative )
import qualified System.FilePath as NativeFilePath ( takeFileName, takeDirectory )
import System.FilePath( splitDirectories, normalise, dropTrailingPathSeparator )
import System.Posix.Files ( isDirectory, getSymbolicLinkStatus )

import Darcs.Util.ByteString ( encodeLocale, decodeLocale )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString       as B

import Data.Binary
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.URL ( isAbsolute, isRelative, isSshNopath )


-- Utilities for use by command implementations

-- | For displaying paths to the user. It should never be used
-- for on-disk patch storage. This adds the "./" for consistency
-- with how repo paths are displayed by 'showPatch' and friends,
-- except for the root path which is displayed as plain ".".
displayPath :: AnchoredPath -> FilePath
displayPath p
  | isRoot p = "."
  | otherwise = anchorPath "." p

-- | Interpret an 'AnchoredPath' as relative the current working
-- directory. Intended for IO operations in the file system.
-- Use with care!
realPath :: AnchoredPath -> FilePath
realPath = anchorPath ""

-- | 'encodeWhite' translates whitespace in filenames to a darcs-specific
--   format (numerical representation according to 'ord' surrounded by
--   backslashes).  Note that backslashes are also escaped since they are used
--   in the encoding.
--
--   > encodeWhite "hello there" == "hello\32\there"
--   > encodeWhite "hello\there" == "hello\92\there"
encodeWhite :: FilePath -> String
encodeWhite (c:cs) | isSpace c || c == '\\' =
    '\\' : show (ord c) ++ "\\" ++ encodeWhite cs
encodeWhite (c:cs) = c : encodeWhite cs
encodeWhite [] = []

-- | 'decodeWhite' interprets the Darcs-specific \"encoded\" filenames
--   produced by 'encodeWhite'
--
--   > decodeWhite "hello\32\there"  == "hello there"
--   > decodeWhite "hello\92\there"  == "hello\there"
--   > decodeWhite "hello\there"   == error "malformed filename"
decodeWhite :: String -> FilePath
decodeWhite cs_ = go cs_ [] False
 where go "" acc True  = reverse acc -- if there was a replace, use new string
       go "" _   False = cs_         -- if not, use input string
       go ('\\':cs) acc _ =
         case break (=='\\') cs of
           (theord, '\\':rest) ->
             go rest (chr (read theord) :acc) True
           _ -> error "malformed filename"
       go (c:cs) acc modified = go cs (c:acc) modified

class FilePathOrURL a where
  toPath :: a -> String

class FilePathOrURL a => FilePathLike a where
  toFilePath :: a -> FilePath

-- | Paths which are relative to the local darcs repository and normalized.
-- Note: These are understood not to have the dot in front.
newtype SubPath      = SubPath FilePath deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath FilePath deriving (Eq, Ord)

-- | This is for situations where a string (e.g. a command line argument)
-- may take the value \"-\" to mean stdin or stdout (which one depends on
-- context) instead of a normal file path.
data AbsolutePathOrStd = AP AbsolutePath | APStd deriving (Eq, Ord)
data AbsoluteOrRemotePath = AbsP AbsolutePath | RmtP String deriving (Eq, Ord)

instance FilePathOrURL AbsolutePath where
  toPath (AbsolutePath x) = x
instance FilePathOrURL SubPath where
  toPath (SubPath x) = x
instance CharLike c => FilePathOrURL [c] where
  toPath = toFilePath

instance FilePathOrURL AbsoluteOrRemotePath where
  toPath (AbsP a) = toPath a
  toPath (RmtP r) = r

instance FilePathLike AbsolutePath where
  toFilePath (AbsolutePath x) = x
instance FilePathLike SubPath where
  toFilePath (SubPath x) = x

class CharLike c where
  toChar :: c -> Char

instance CharLike Char where
  toChar = id

instance CharLike c => FilePathLike [c] where
  toFilePath = map toChar

-- | Make the second path relative to the first, if possible
makeSubPathOf :: AbsolutePath -> AbsolutePath -> Maybe SubPath
makeSubPathOf (AbsolutePath p1) (AbsolutePath p2) =
 -- The slash prevents "foobar" from being treated as relative to "foo"
 if p1 == p2 || (p1 ++ "/") `isPrefixOf` p2
    then Just $ SubPath $ drop (length p1 + 1) p2
    else Nothing

simpleSubPath :: FilePath -> Maybe SubPath
simpleSubPath x | null x = error "simpleSubPath called with empty path"
                | isRelative x = Just $ SubPath $ FilePath.normalise $ pathToPosix x
                | otherwise = Nothing

-- | Ensure directory exists and is not a symbolic link.
doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = do
    x <- tryJust (\x -> if isDoesNotExistError x then Just () else Nothing) $
        isDirectory <$> getSymbolicLinkStatus f
    return $ case x of
        Left () -> False
        Right y -> y

doesPathExist :: FilePath -> IO Bool
doesPathExist p = do
   dir_exists <- doesDirectoryExist p
   file_exists <- doesFileExist p
   return $ dir_exists || file_exists

-- | Interpret a possibly relative path wrt the current working directory.
ioAbsolute :: FilePath -> IO AbsolutePath
ioAbsolute dir =
    do isdir <- doesDirectoryReallyExist dir
       here <- getCurrentDirectory
       if isdir
         then bracket_ (setCurrentDirectory dir)
                       (setCurrentDirectory $ toFilePath here)
                       getCurrentDirectory
         else let super_dir = case NativeFilePath.takeDirectory dir of
                                "" ->  "."
                                d  -> d
                  file = NativeFilePath.takeFileName dir
              in do abs_dir <- if dir == super_dir
                               then return $ AbsolutePath dir
                               else ioAbsolute super_dir
                    return $ makeAbsolute abs_dir file

-- | Take an absolute path and a string representing a (possibly relative)
-- path and combine them into an absolute path. If the second argument is
-- already absolute, then the first argument gets ignored. This function also
-- takes care that the result is converted to Posix convention and
-- normalized. Also, parent directories (\"..\") at the front of the string
-- argument get canceled out against trailing directory parts of the
-- absolute path argument.
--
-- Regarding the last point, someone more familiar with how these functions
-- are used should verify that this is indeed necessary or at least useful.
makeAbsolute :: AbsolutePath -> FilePath -> AbsolutePath
makeAbsolute a dir = if not (null dir) && isAbsolute dir
                     then AbsolutePath (normSlashes dir')
                     else ma a dir'
  where
    dir' = FilePath.normalise $ pathToPosix dir
    -- Why do we care to reduce ".." here?
    -- Why not do this throughout the whole path, i.e. "x/y/../z" -> "x/z" ?
    ma here ('.':'.':'/':r) = ma (takeDirectory here) r
    ma here ".." = takeDirectory here
    ma here "." = here
    ma here "" = here
    ma here r = here /- ('/':r)

(/-) :: AbsolutePath -> String -> AbsolutePath
x /- ('/':r) = x /- r
(AbsolutePath "/") /- r = AbsolutePath ('/':simpleClean r)
(AbsolutePath x) /- r = AbsolutePath (x++'/':simpleClean r)

-- | Convert to posix, remove trailing slashes, and (under Posix)
-- reduce multiple leading slashes to one.
simpleClean :: String -> String
simpleClean = normSlashes . reverse . dropWhile (=='/') . reverse . pathToPosix

makeAbsoluteOrStd :: AbsolutePath -> String -> AbsolutePathOrStd
makeAbsoluteOrStd _ "-" = APStd
makeAbsoluteOrStd a p = AP $ makeAbsolute a p

stdOut :: AbsolutePathOrStd
stdOut = APStd

ioAbsoluteOrStd :: String -> IO AbsolutePathOrStd
ioAbsoluteOrStd "-" = return APStd
ioAbsoluteOrStd p = AP `fmap` ioAbsolute p

-- | Execute either the first or the second argument action, depending on
-- whether the given path is an 'AbsolutePath' or stdin/stdout.
useAbsoluteOrStd :: (AbsolutePath -> a) -> a -> AbsolutePathOrStd -> a
useAbsoluteOrStd _ f APStd = f
useAbsoluteOrStd f _ (AP x) = f x

ioAbsoluteOrRemote :: String -> IO AbsoluteOrRemotePath
ioAbsoluteOrRemote p = do
  isdir <- doesDirectoryExist p
  if not isdir
     then return $ RmtP $
          case () of _ | isSshNopath p    -> p++"."
                       | "/" `isSuffixOf` p -> init p
                       | otherwise          -> p
     else AbsP `fmap` ioAbsolute p

isRemote :: AbsoluteOrRemotePath -> Bool
isRemote (RmtP _) = True
isRemote _ = False

takeDirectory :: AbsolutePath -> AbsolutePath
takeDirectory (AbsolutePath x) =
    case reverse $ drop 1 $ dropWhile (/='/') $ reverse x of
    "" -> AbsolutePath "/"
    x' -> AbsolutePath x'

instance Show AbsolutePath where
 show = show . toFilePath
instance Show SubPath where
 show = show . toFilePath
instance Show AbsolutePathOrStd where
    show (AP a) = show a
    show APStd = "standard input/output"
instance Show AbsoluteOrRemotePath where
    show (AbsP a) = show a
    show (RmtP r) = show r

-- | Normalize the path separator to Posix style (slash, not backslash).
-- This only affects Windows systems.
pathToPosix :: FilePath -> FilePath
pathToPosix = map convert where
#ifdef WIN32
  convert '\\' = '/'
#endif
  convert c = c

-- | Reduce multiple leading slashes to one. This only affects Posix systems.
normSlashes :: FilePath -> FilePath
#ifndef WIN32
-- multiple slashes in front are ignored under Posix
normSlashes ('/':p) = '/' : dropWhile (== '/') p
#endif
normSlashes p = p

getCurrentDirectory :: IO AbsolutePath
getCurrentDirectory = AbsolutePath `fmap` Workaround.getCurrentDirectory

setCurrentDirectory :: FilePathLike p => p -> IO ()
setCurrentDirectory = System.Directory.setCurrentDirectory . toFilePath

{-|
  What is a malicious path?

  A spoofed path is a malicious path.

  1. Darcs only creates explicitly relative paths (beginning with @\".\/\"@),
     so any not explicitly relative path is surely spoofed.

  2. Darcs normalizes paths so they never contain @\"\/..\/\"@, so paths with
     @\"\/..\/\"@ are surely spoofed.

  A path to a darcs repository's meta data can modify \"trusted\" patches or
  change safety defaults in that repository, so we check for paths
  containing @\"\/_darcs\/\"@ which is the entry to darcs meta data.

  To do?

  * How about get repositories?

  * Would it be worth adding a --semi-safe-paths option for allowing
    changes to certain preference files (_darcs\/prefs\/) in sub
    repositories'?

  TODO:
    Properly review the way we handle paths on Windows - it's not enough
    to just use the OS native concept of path separator. Windows often
    accepts both path separators, and repositories always use the UNIX
    separator anyway.
-}

isMaliciousSubPath :: String -> Bool
isMaliciousSubPath fp =
    not (FilePath.isRelative fp) || isGenerallyMalicious fp

isGenerallyMalicious :: String -> Bool
isGenerallyMalicious fp =
    splitDirectories fp `contains_any` [ "..", darcsdir ]
 where
    contains_any a b = not . null $ intersect a b

-- | Iteratively tries find first non-existing path generated by
-- buildName, it feeds to buildName the number starting with -1.  When
-- it generates non-existing path and it isn't first, it displays the
-- message created with buildMsg. Usually used for generation of the
-- name like <path>_<number> when <path> already exist
-- (e.g. darcs.net_0).
getUniquePathName :: Bool -> (FilePath -> String) -> (Int -> FilePath) -> IO FilePath
getUniquePathName talkative buildMsg buildName = go (-1)
 where
  go :: Int -> IO FilePath
  go i = do
    exists <- doesPathExist thename
    if not exists
       then do when (i /= -1 && talkative) $ putStrLn $ buildMsg thename
               return thename
       else go $ i+1
    where thename = buildName i

-------------------------------
-- AnchoredPath utilities
--

newtype Name = Name { unName :: B.ByteString } deriving (Binary, Eq, Show, Ord)

-- | This is a type of "sane" file paths. These are always canonic in the sense
-- that there are no stray slashes, no ".." components and similar. They are
-- usually used to refer to a location within a Tree, but a relative filesystem
-- path works just as well. These are either constructed from individual name
-- components (using "appendPath", "catPaths" and "makeName"), or converted
-- from a FilePath ("floatPath" -- but take care when doing that).
newtype AnchoredPath = AnchoredPath [Name] deriving (Binary, Eq, Show, Ord)

-- | Check whether a path is a prefix of another path.
isPrefix :: AnchoredPath -> AnchoredPath -> Bool
(AnchoredPath a) `isPrefix` (AnchoredPath b) = a `isPrefixOf` b

-- | Append an element to the end of a path.
appendPath :: AnchoredPath -> Name -> AnchoredPath
appendPath (AnchoredPath p) n = AnchoredPath $ p ++ [n]

-- | Catenate two paths together. Not very safe, but sometimes useful
-- (e.g. when you are representing paths relative to a different point than a
-- Tree root).
catPaths :: AnchoredPath -> AnchoredPath -> AnchoredPath
catPaths (AnchoredPath p) (AnchoredPath n) = AnchoredPath (p ++ n)

-- | Get parent (path) of a given path. foo/bar/baz -> foo/bar
parent :: AnchoredPath -> Maybe AnchoredPath
parent (AnchoredPath []) = Nothing
parent (AnchoredPath x) = Just (AnchoredPath (init x))

-- | List all parents of a given path. foo/bar/baz -> [.,foo, foo/bar]
parents :: AnchoredPath -> [AnchoredPath]
parents (AnchoredPath []) = [] -- root has no parents
parents (AnchoredPath xs) = map AnchoredPath $ inits $ init xs

-- | If the patch is under a directory, split into Right of the first component
-- (which must be a directory name) and the rest of teh path. Otherwise
-- return Left of the single component.
-- This function is *undefined* on the root path (which has no components).
breakOnDir :: AnchoredPath -> Either Name (Name, AnchoredPath)
breakOnDir (AnchoredPath []) = error "breakOnDir called on root"
breakOnDir (AnchoredPath (n:[])) = Left n
breakOnDir (AnchoredPath (n:ns)) = Right (n, AnchoredPath ns)

-- | Take a "root" directory and an anchored path and produce a full
-- 'FilePath'. Moreover, you can use @anchorPath \"\"@ to get a relative
-- 'FilePath'.
anchorPath :: FilePath -> AnchoredPath -> FilePath
anchorPath dir p = dir FilePath.</> decodeLocale (flatten p)
{-# INLINE anchorPath #-}

name2fp :: Name -> FilePath
name2fp (Name ps) = decodeLocale ps

-- FIXME returning "." for the root is wrong
flatten :: AnchoredPath -> BC.ByteString
flatten (AnchoredPath []) = BC.singleton '.'
flatten (AnchoredPath p) = BC.intercalate (BC.singleton '/') [n | (Name n) <- p]

-- | Make a 'Name' from a 'String'. If the input 'String'
-- is invalid, that is, "", ".", "..", or contains a '/', return 'Left'
-- with an error message.
makeName :: String -> Either String Name
makeName = rawMakeName . encodeLocale

-- | Make a 'Name' from a 'String'. If the input 'String'
-- is invalid, that is, "", ".", "..", or contains a '/', call error.
internalMakeName :: String -> Name
internalMakeName = either error id . rawMakeName . encodeLocale

-- | Take a relative FilePath and turn it into an AnchoredPath. This is a
-- partial function. Basically, by using floatPath, you are testifying that the
-- argument is a path relative to some common root -- i.e. the root of the
-- associated "Tree" object. In particular, the input path may not contain any
-- ocurrences of "." or ".." after normalising. You should sanitize any
-- FilePaths before you declare them "good" by converting into AnchoredPath
-- (using this function), especially if the FilePath come from any external
-- source (command line, file, environment, network, etc)
floatPath :: FilePath -> AnchoredPath
floatPath =
    AnchoredPath . map internalMakeName . filter sensible .
    splitDirectories . normalise . dropTrailingPathSeparator
  where
    sensible s = s `notElem` ["", "."]

anchoredRoot :: AnchoredPath
anchoredRoot = AnchoredPath []

-- | A view on 'AnchoredPath's.
parentChild :: AnchoredPath -> Maybe (AnchoredPath, Name)
parentChild (AnchoredPath []) = Nothing
parentChild (AnchoredPath xs) = Just (AnchoredPath (init xs), last xs)

-- | Replace the second arg's parent with the first arg.
replaceParent :: AnchoredPath -> AnchoredPath -> Maybe AnchoredPath
replaceParent (AnchoredPath xs) p =
  case parentChild p of
    Nothing -> Nothing
    Just (_,x) -> Just (AnchoredPath (xs ++ [x]))

-- | Make a 'Name' from a 'B.ByteString'.
rawMakeName :: B.ByteString -> Either String Name
rawMakeName s
  | isBadName s =
      Left $ "'"++decodeLocale s++"' is not a valid AnchoredPath component name"
  | otherwise = Right (Name s)

isBadName :: B.ByteString -> Bool
isBadName n = hasPathSeparator n || n `elem` forbiddenNames

-- It would be nice if we could add BC.pack "_darcs" to the list, however
-- "_darcs" could be a valid file or dir name if not inside the top level
-- directory.
forbiddenNames :: [B.ByteString]
forbiddenNames = [BC.empty, BC.pack ".", BC.pack ".."]

hasPathSeparator :: B.ByteString -> Bool
hasPathSeparator = BC.elem '/'

eqAnycase :: Name -> Name -> Bool
eqAnycase (Name a) (Name b) = BC.map toLower a == BC.map toLower b

encodeWhiteName :: Name -> B.ByteString
encodeWhiteName = encodeLocale . encodeWhite . decodeLocale . unName

data CorruptPatch = CorruptPatch String deriving (Eq, Typeable)
instance Exception CorruptPatch
instance Show CorruptPatch where show (CorruptPatch s) = s

decodeWhiteName :: B.ByteString -> Name
decodeWhiteName =
  either (throw . CorruptPatch) id .
  rawMakeName . encodeLocale . decodeWhite . decodeLocale

-- | The effect of renaming on paths.
-- The first argument is the old path, the second is the new path,
-- and the third is the possibly affected path we are interested in.
movedirfilename :: AnchoredPath -> AnchoredPath -> AnchoredPath -> AnchoredPath
movedirfilename (AnchoredPath old) newp@(AnchoredPath new) orig@(AnchoredPath path) =
  case stripPrefix old path of
    Just [] -> newp -- optimization to avoid allocation in this case
    Just rest -> AnchoredPath (new ++ rest)
    Nothing -> orig -- old is not a prefix => no change

-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath] -> AnchoredPath -> t -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files


-- | Transform a SubPath into an AnchoredPath.
floatSubPath :: SubPath -> AnchoredPath
floatSubPath = floatPath . toFilePath

-- | Is the given path in (or equal to) the _darcs metadata directory?
inDarcsdir :: AnchoredPath -> Bool
inDarcsdir (AnchoredPath (x:_)) | x == darcsdirName = True
inDarcsdir _ = False

darcsdirName :: Name
darcsdirName = internalMakeName darcsdir

isRoot :: AnchoredPath -> Bool
isRoot (AnchoredPath xs) = null xs
