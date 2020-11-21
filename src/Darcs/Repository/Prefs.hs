--  Copyright (C) 2002-2003 David Roundy
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

module Darcs.Repository.Prefs
    ( addToPreflist
    , deleteSources
    , getPreflist
    , setPreflist
    , getGlobal
    , environmentHelpHome
    , defaultrepo
    , getDefaultRepo
    , addRepoSource
    , getPrefval
    , setPrefval
    , changePrefval
    , defPrefval
    , writeDefaultPrefs
    , boringRegexps
    , isBoring
    , FileType(..)
    , filetypeFunction
    , getCaches
    , globalCacheDir
    , globalPrefsDirDoc
    , globalPrefsDir
    , getMotd
    , showMotd
    , prefsUrl
    , prefsDirPath
    , prefsFilePath
    , getPrefLines -- exported for darcsden, don't remove
    -- * documentation of prefs files
    , prefsFilesHelp
    ) where

import Darcs.Prelude

import Control.Exception ( catch )
import Control.Monad ( unless, when, liftM )
import Data.Char ( toUpper )
import Data.List ( nub, isPrefixOf, union, lookup )
import Data.Maybe ( isJust, fromMaybe, mapMaybe, catMaybes, maybeToList )
import qualified Control.Exception as C
import qualified Data.ByteString       as B  ( empty, null, hPut, ByteString )
import qualified Data.ByteString.Char8 as BC ( unpack )
import System.Directory ( getAppUserDataDirectory, doesDirectoryExist,
                          createDirectory, doesFileExist )
import System.Environment ( getEnvironment )
import System.FilePath.Posix ( normalise, dropTrailingPathSeparator, (</>) )
import System.IO.Error ( isDoesNotExistError, catchIOError )
import System.IO ( stdout, stderr )
import System.Info ( os )
import System.Posix.Files ( getFileStatus, fileOwner )
import Text.Regex ( Regex, mkRegex, matchRegex )

import Darcs.Repository.Cache ( Cache, mkCache, CacheType(..), CacheLoc(..),
                                WritableOrNot(..) )
import Darcs.Util.External ( gzFetchFilePS , fetchFilePS, Cachable(..))
import Darcs.Repository.Flags
    ( UseCache (..)
    , DryRun (..)
    , SetDefault (..)
    , InheritDefault (..)
    , RemoteRepos (..)
    )
import Darcs.Util.Lock( readTextFile, writeTextFile )
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Global ( darcsdir, debugMessage )
import Darcs.Util.Path ( AbsolutePath, ioAbsolute, toFilePath,
                         getCurrentDirectory )
import Darcs.Util.Printer( hPutDocLn, text )
import Darcs.Util.URL ( isValidLocalPath )
import Darcs.Util.File ( osxCacheDir, xdgCacheDir, removeFileMayNotExist )

windows,osx :: Bool
windows = "mingw" `isPrefixOf` os -- GHC under Windows is compiled with mingw
osx     = os == "darwin"

writeDefaultPrefs :: IO ()
writeDefaultPrefs = do
    setPreflist "boring" defaultBoring
    setPreflist "binaries" defaultBinaries
    setPreflist "motd" []

defaultBoring :: [String]
defaultBoring = map ("# " ++) boringFileInternalHelp ++
    [ ""
    , "### compiler and interpreter intermediate files"
    , "# haskell (ghc) interfaces"
    , "\\.hi$", "\\.hi-boot$", "\\.o-boot$"
    , "# object files"
    , "\\.o$","\\.o\\.cmd$"
    , "# profiling haskell"
    , "\\.p_hi$", "\\.p_o$"
    , "# haskell program coverage resp. profiling info"
    , "\\.tix$", "\\.prof$"
    , "# fortran module files"
    , "\\.mod$"
    , "# linux kernel"
    , "\\.ko\\.cmd$","\\.mod\\.c$"
    , "(^|/)\\.tmp_versions($|/)"
    , "# *.ko files aren't boring by default because they might"
    , "# be Korean translations rather than kernel modules"
    , "# \\.ko$"
    , "# python, emacs, java byte code"
    , "\\.py[co]$", "\\.elc$","\\.class$"
    , "# objects and libraries; lo and la are libtool things"
    , "\\.(obj|a|exe|so|lo|la)$"
    , "# compiled zsh configuration files"
    , "\\.zwc$"
    , "# Common LISP output files for CLISP and CMUCL"
    , "\\.(fas|fasl|sparcf|x86f)$"
    , ""
    , "### build and packaging systems"
    , "# cabal intermediates"
    , "\\.installed-pkg-config"
    , "\\.setup-config"
    , "# standard cabal build dir, might not be boring for everybody"
    , "# ^dist(/|$)"
    , "# autotools"
    , "(^|/)autom4te\\.cache($|/)", "(^|/)config\\.(log|status)$"
    , "# microsoft web expression, visual studio metadata directories"
    , "\\_vti_cnf$"
    , "\\_vti_pvt$"
    , "# gentoo tools"
    , "\\.revdep-rebuild.*"
    , "# generated dependencies"
    , "^\\.depend$"
    , ""
    , "### version control systems"
    , "# cvs"
    , "(^|/)CVS($|/)","\\.cvsignore$"
    , "# cvs, emacs locks"
    , "^\\.#"
    , "# rcs"
    , "(^|/)RCS($|/)", ",v$"
    , "# subversion"
    , "(^|/)\\.svn($|/)"
    , "# mercurial"
    , "(^|/)\\.hg($|/)"
    , "# git"
    , "(^|/)\\.git($|/)"
    , "# bzr"
    , "\\.bzr$"
    , "# sccs"
    , "(^|/)SCCS($|/)"
    , "# darcs"
    , "(^|/)"++darcsdir++"($|/)", "(^|/)\\.darcsrepo($|/)"
    , "# gnu arch"
    , "(^|/)(\\+|,)"
    , "(^|/)vssver\\.scc$"
    , "\\.swp$","(^|/)MT($|/)"
    , "(^|/)\\{arch\\}($|/)","(^|/).arch-ids($|/)"
    , "# bitkeeper"
    , "(^|/)BitKeeper($|/)","(^|/)ChangeSet($|/)"
    , ""
    , "### miscellaneous"
    , "# backup files"
    , "~$","\\.bak$","\\.BAK$"
    , "# patch originals and rejects"
    , "\\.orig$", "\\.rej$"
    , "# X server"
    , "\\..serverauth.*"
    , "# image spam"
    , "\\#", "(^|/)Thumbs\\.db$"
    , "# vi, emacs tags"
    , "(^|/)(tags|TAGS)$"
    , "#(^|/)\\.[^/]"
    , "# core dumps"
    , "(^|/|\\.)core$"
    , "# partial broken files (KIO copy operations)"
    , "\\.part$"
    , "# waf files, see http://code.google.com/p/waf/"
    , "(^|/)\\.waf-[[:digit:].]+-[[:digit:]]+($|/)"
    , "(^|/)\\.lock-wscript$"
    , "# mac os finder"
    , "(^|/)\\.DS_Store$"
    , "# emacs saved sessions (desktops)"
    , "(^|.*/)\\.emacs\\.desktop(\\.lock)?$"
    , " # stack"
    , "(^|/)\\.stack-work($|/)"
    ]

boringFileInternalHelp :: [String]
boringFileInternalHelp =
    [ "This file contains a list of extended regular expressions, one per"
    , "line. A file path matching any of these expressions will be filtered"
    , "out during `darcs add`, or when the `--look-for-adds` flag is passed"
    , "to `darcs whatsnew` and `record`. The entries in "
        ++ globalPrefsDirDoc ++ "boring (if"
    , "it exists) supplement those in this file."
    , ""
    , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
    , "See regex(7) for a description of extended regular expressions."
    ]

-- | The path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows.
globalPrefsDir :: IO (Maybe FilePath)
globalPrefsDir = do
    env <- getEnvironment
    case lookup "DARCS_TESTING_PREFS_DIR" env of
        Just d -> return (Just d)
        Nothing -> Just `fmap` getAppUserDataDirectory "darcs"
                   `catchall` return Nothing

-- | The relative path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows. This is used for online documentation.
globalPrefsDirDoc :: String
globalPrefsDirDoc | windows   = "%APPDATA%\\darcs\\"
                  | otherwise = "~/.darcs/"

environmentHelpHome :: ([String], [String])
environmentHelpHome =
    ( ["HOME", "APPDATA"]
    , [ "Per-user preferences are set in $HOME/.darcs (on Unix) or"
      , "%APPDATA%/darcs (on Windows).  This is also the default location of"
      , "the cache."
      ]
    )

getGlobal :: String -> IO [String]
getGlobal f = do
    dir <- globalPrefsDir
    case dir of
        (Just d) -> getPreffile $ d </> f
        Nothing -> return []

globalCacheDir :: IO (Maybe FilePath)
globalCacheDir | windows   = ((</> "cache2") `fmap`) `fmap` globalPrefsDir
               | osx       = ((</> "darcs") `fmap`) `fmap` osxCacheDir
               | otherwise = ((</> "darcs") `fmap`) `fmap` xdgCacheDir

-- |tryMakeBoringRegexp attempts to create a Regex from a given String. The
-- evaluation is forced, to ensure any malformed exceptions are thrown here,
-- and not later.
tryMakeBoringRegexp :: String -> IO (Maybe Regex)
tryMakeBoringRegexp input = regex `C.catch` handleBadRegex
  where
    regex = C.evaluate (Just $! mkRegex input)

    handleBadRegex :: C.SomeException -> IO (Maybe Regex)
    handleBadRegex _ = hPutDocLn stderr warning >> return Nothing

    warning = text $ "Warning: Ignored invalid boring regex: " ++ input

-- |boringRegexps returns a list of the boring regexps, from the local and
-- global prefs/boring files. Any invalid regexps are filtered, preventing an
-- exception in (potentially) pure code, when the regexps are used.
boringRegexps :: IO [Regex]
boringRegexps = do
    borefile <- defPrefval "boringfile" (darcsdir ++ "/prefs/boring")
    localBores <- getPrefLines borefile `catchall` return []
    globalBores <- getGlobal "boring"
    liftM catMaybes $ mapM tryMakeBoringRegexp $ localBores ++ globalBores

isBoring :: IO (FilePath -> Bool)
isBoring = do
  regexps <- boringRegexps
  return $ \file -> any (\r -> isJust $ matchRegex r file) regexps

noncomments :: [String] -> [String]
noncomments = filter nonComment
  where
    nonComment "" = False
    nonComment ('#' : _) = False
    nonComment _ = True

getPrefLines :: FilePath -> IO [String]
getPrefLines f = removeCRsCommentsAndConflicts `fmap` readTextFile f
  where
    removeCRsCommentsAndConflicts =
        filter notconflict . noncomments . map stripCr
    startswith [] _ = True
    startswith (x : xs) (y : ys) = x == y && startswith xs ys
    startswith _ _ = False
    notconflict l
        | startswith "v v v v v v v" l = False
        | startswith "*************" l = False
        | startswith "^ ^ ^ ^ ^ ^ ^" l = False
        | otherwise = True
    stripCr ""     = ""
    stripCr "\r"   = ""
    stripCr (c : cs) = c : stripCr cs

doNormalise :: FilePath -> FilePath
doNormalise = dropTrailingPathSeparator . normalise

data FileType = BinaryFile
              | TextFile
              deriving (Eq)

-- | The lines that will be inserted into @_darcs/prefs/binaries@ when
-- @darcs init@ is run.  Hence, a list of comments, blank lines and
-- regular expressions (ERE dialect).
--
-- Note that while this matches .gz and .GZ, it will not match .gZ,
-- i.e. it is not truly case insensitive.
defaultBinaries :: [String]
defaultBinaries = map ("# "++) binariesFileInternalHelp ++
    [ "\\." ++ regexToMatchOrigOrUpper e ++ "$" | e <- extensions ]
  where
    regexToMatchOrigOrUpper e = "(" ++ e ++ "|" ++ map toUpper e ++ ")"
    extensions =
        [ "a"
        , "bmp"
        , "bz2"
        , "doc"
        , "elc"
        , "exe"
        , "gif"
        , "gz"
        , "iso"
        , "jar"
        , "jpe?g"
        , "mng"
        , "mpe?g"
        , "p[nbgp]m"
        , "pdf"
        , "png"
        , "pyc"
        , "so"
        , "tar"
        , "tgz"
        , "tiff?"
        , "z"
        , "zip"
        ]

binariesFileInternalHelp :: [String]
binariesFileInternalHelp =
    [ "This file contains a list of extended regular expressions, one per"
    , "line.  A file path matching any of these expressions is assumed to"
    , "contain binary data (not text). The entries in "
        ++ globalPrefsDirDoc ++ "binaries (if"
    , "it exists) supplement those in this file."
    , ""
    , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
    , "See regex(7) for a description of extended regular expressions."
    ]

filetypeFunction :: IO (FilePath -> FileType)
filetypeFunction = do
    binsfile <- defPrefval "binariesfile" (darcsdir ++ "/prefs/binaries")
    bins <- getPrefLines binsfile
            `catch`
            (\e -> if isDoesNotExistError e then return [] else ioError e)
    gbs <- getGlobal "binaries"
    let binaryRegexes = map mkRegex (bins ++ gbs)
        isBinary f = any (\r -> isJust $ matchRegex r f) binaryRegexes
        ftf f = if isBinary $ doNormalise f then BinaryFile else TextFile
    return ftf

findPrefsDirectory :: IO (Maybe String)
findPrefsDirectory = do
    inDarcsRepo <- doesDirectoryExist darcsdir
    return $ if inDarcsRepo
                 then Just $ darcsdir ++ "/prefs/"
                 else Nothing

withPrefsDirectory :: (String -> IO ()) -> IO ()
withPrefsDirectory job = findPrefsDirectory >>= maybe (return ()) job

addToPreflist :: String -> String -> IO ()
addToPreflist pref value = withPrefsDirectory $ \prefs -> do
    hasprefs <- doesDirectoryExist prefs
    unless hasprefs $ createDirectory prefs
    pl <- getPreflist pref
    writeTextFile (prefs ++ pref) . unlines $ union [value] pl

getPreflist :: String -> IO [String]
getPreflist p = findPrefsDirectory >>=
                maybe (return []) (\prefs -> getPreffile $ prefs ++ p)

getPreffile :: FilePath -> IO [String]
getPreffile f = do
    hasprefs <- doesFileExist f
    if hasprefs then getPrefLines f else return []

setPreflist :: String -> [String] -> IO ()
setPreflist p ls = withPrefsDirectory $ \prefs -> do
    haspref <- doesDirectoryExist prefs
    when haspref $
        writeTextFile (prefs ++ p) (unlines ls)

defPrefval :: String -> String -> IO String
defPrefval p d = fromMaybe d `fmap` getPrefval p

getPrefval :: String -> IO (Maybe String)
getPrefval p = do
    pl <- getPreflist prefsDir
    return $ case map snd $ filter ((== p) . fst) $ map (break (== ' ')) pl of
                 [val] -> case words val of
                    [] -> Nothing
                    _ -> Just $ tail val
                 _ -> Nothing

setPrefval :: String -> String -> IO ()
setPrefval p v = do
    pl <- getPreflist prefsDir
    setPreflist prefsDir $ updatePrefVal pl p v

updatePrefVal :: [String] -> String -> String -> [String]
updatePrefVal prefList p newVal =
    filter ((/= p) . fst . break (== ' ')) prefList ++ [p ++ " " ++ newVal]

changePrefval :: String -> String -> String -> IO ()
changePrefval p f t = do
    pl <- getPreflist prefsDir
    ov <- getPrefval p
    let newval = maybe t (\old -> if old == f then t else old) ov
    setPreflist prefsDir $ updatePrefVal pl p newval

fixRepoPath :: String -> IO FilePath
fixRepoPath p
    | isValidLocalPath p = toFilePath `fmap` ioAbsolute p
    | otherwise = return p

defaultrepo :: RemoteRepos -> AbsolutePath -> [String] -> IO [String]
defaultrepo (RemoteRepos rrepos) _ [] =
  do case rrepos of
       [] -> maybeToList `fmap` getDefaultRepo
       rs -> mapM fixRepoPath rs
defaultrepo _ _ r = return r

getDefaultRepo :: IO (Maybe String)
getDefaultRepo = do
    defaults <- getPreflist defaultRepoPref
    case defaults of
         [] -> return Nothing
         (d : _) -> Just `fmap` fixRepoPath d

defaultRepoPref :: String
defaultRepoPref = "defaultrepo"

-- | addRepoSource adds a new entry to _darcs/prefs/repos and sets it as default
--   in _darcs/prefs/defaultrepo, unless --no-set-default or --dry-run is passed,
--   or it is the same repository as the current one.
addRepoSource :: String
              -> DryRun
              -> RemoteRepos
              -> SetDefault
              -> InheritDefault
              -> Bool
              -> IO ()
addRepoSource r isDryRun (RemoteRepos rrepos) setDefault inheritDefault isInteractive = (do
    olddef <- getPreflist defaultRepoPref
    newdef <- newDefaultRepo
    let shouldDoIt = null noSetDefault && greenLight
        greenLight = shouldAct && not rIsTmp && (olddef /= [newdef] || olddef == [])
    -- the nuance here is that we should only notify when the reason we're not
    -- setting default is the --no-set-default flag, not the various automatic
    -- show stoppers
    if shouldDoIt
       then setPreflist defaultRepoPref [newdef]
       else when (True `notElem` noSetDefault && greenLight && inheritDefault == NoInheritDefault) $
                putStr . unlines $ setDefaultMsg
    addToPreflist "repos" newdef) `catchall` return ()
  where
    shouldAct = isDryRun == NoDryRun
    rIsTmp = r `elem` rrepos
    noSetDefault = case setDefault of
                       NoSetDefault x -> [x]
                       _ -> []
    setDefaultMsg =
        [ "By the way, to change the default remote repository to"
        , "      " ++ r ++ ","
        , "you can " ++
          (if isInteractive then "quit now and " else "") ++
          "issue the same command with the --set-default flag."
        ]
    newDefaultRepo :: IO String
    newDefaultRepo = case inheritDefault of
      YesInheritDefault -> getRemoteDefaultRepo
      NoInheritDefault -> return r
    -- TODO It would be nice if --inherit-default could be made to work with
    -- arbitrary remote repos; for security reasons we currently allow only
    -- repos on the same host which must also be owned by ourselves. This is
    -- because the defaultrepo file is read and written as a text file, and
    -- therefore encoded in the user's locale encoding. See
    -- http://bugs.darcs.net/issue2627 for a more detailed discussion.
    getRemoteDefaultRepo
      | isValidLocalPath r = do
          sameOwner r "." >>= \case
            True -> do
              defs <-
                getPreffile (r </> darcsdir </> "prefs/defaultrepo")
                `catchIOError`
                const (return [r])
              case defs of
                defrepo:_ -> do
                  debugMessage "using defaultrepo of remote"
                  return defrepo
                [] -> return r
            False -> return r
      | otherwise = return r
    sameOwner p q =
      (==) <$> (fileOwner <$> getFileStatus p) <*> (fileOwner <$> getFileStatus q)

-- | delete references to other repositories.
--   Used when cloning to a ssh destination.
--   Assume the current working dir is the repository.
deleteSources :: IO ()
deleteSources = do let prefsdir = darcsdir ++ "/prefs/"
                   removeFileMayNotExist (prefsdir ++ "sources")
                   removeFileMayNotExist (prefsdir ++ "repos")

getCaches :: UseCache -> String -> IO Cache
getCaches useCache repodir = do
    here <- parsehs `fmap` getPreffile sourcesFile
    there <- (parsehs . lines . BC.unpack)
             `fmap`
             (gzFetchFilePS (repodir </> sourcesFile) Cachable
              `catchall` return B.empty)
    globalcachedir <- globalCacheDir
    let globalcache = if nocache
                          then []
                          else case globalcachedir of
                              Nothing -> []
                              Just d -> [Cache Directory Writable d]
    globalsources <- parsehs `fmap` getGlobal "sources"
    thisdir <- getCurrentDirectory
    let thisrepo = [Cache Repo Writable $ toFilePath thisdir]
        thatrepo = [Cache Repo NotWritable repodir]
        tempCache = nub $ thisrepo ++ globalcache ++ globalsources ++ here
                          ++ thatrepo ++ filterExternalSources there
    return $ mkCache tempCache
  where
    sourcesFile = darcsdir ++ "/prefs/sources"

    parsehs = mapMaybe readln . noncomments

    readln l
        | "repo:" `isPrefixOf` l = Just (Cache Repo NotWritable (drop 5 l))
        | nocache = Nothing
        | "cache:" `isPrefixOf` l = Just (Cache Directory Writable (drop 6 l))
        | "readonly:" `isPrefixOf` l =
            Just (Cache Directory NotWritable (drop 9 l))
        | otherwise = Nothing

    nocache = useCache == NoUseCache

    filterExternalSources there =
        if isValidLocalPath repodir
            then there
            else filter (not . isValidLocalPath . cacheSource) there

-- | Fetch and return the message of the day for a given repository.
getMotd :: String -> IO B.ByteString
getMotd repo = fetchFilePS motdPath (MaxAge 600) `catchall` return B.empty
  where
    motdPath = repo ++ "/" ++ darcsdir ++ "/prefs/motd"

-- | Display the message of the day for a given repository,
showMotd :: String -> IO ()
showMotd repo = do
    motd <- getMotd repo
    unless (B.null motd) $ do
        B.hPut stdout motd
        putStrLn $ replicate 22 '*'

prefsUrl :: FilePath -> String
prefsUrl r = r ++ "/"++darcsdir++"/prefs"

prefsDir :: FilePath
prefsDir = "prefs"

prefsDirPath :: FilePath
prefsDirPath = darcsdir </> prefsDir

prefsFilePath :: FilePath
prefsFilePath = prefsDirPath </> "prefs"

prefsFilesHelp :: [(String,String)]
prefsFilesHelp  =
    [ ("motd", unlines
      [ "The `_darcs/prefs/motd` file may contain a 'message of the day' which"
      , "will be displayed to users who clone or pull from the repository without"
      , "the `--quiet` option."])
    , ("email", unlines
      [ "The `_darcs/prefs/email` file is used to provide the e-mail address for"
      , "your repository that others will use when they `darcs send` a patch back"
      , "to you. The contents of the file should simply be an e-mail address."])
    , ("post", unlines
      [ "If `_darcs/prefs/post` exists in the target repository, `darcs send ` will"
      , "upload to the URL contained in that file, which may either be a `mailto:`"
      , "URL, or an `http://` URL. In the latter case, the patch is posted to that URL."])
    , ("author", unlines
      [ "The `_darcs/prefs/author` file contains the email address (or name) to"
      , "be used as the author when patches are recorded in this repository,"
      , "e.g. `David Roundy <droundy@abridgegame.org>`. This file overrides the"
      , "contents of the environment variables `$DARCS_EMAIL` and `$EMAIL`."])
    , ("defaults", unlines
      [ "Default options for darcs commands. Each line of this file has the"
      , "following form:"
      , ""
      , "    COMMAND FLAG VALUE"
      , ""
      , "where `COMMAND` is either the name of the command to which the default"
      , "applies, or `ALL` to indicate that the default applies to all commands"
      , "accepting that flag. The `FLAG` term is the name of the long argument"
      , "option with or without the `--`, i.e. `verbose` or `--verbose`."
      , "Finally, the `VALUE` option can be omitted if the flag does not involve"
      , "a value. If the value has spaces in it, use single quotes, not double"
      , "quotes, to surround it. Each line only takes one flag. To set multiple"
      , "defaults for the same command (or for `ALL` commands), use multiple lines."
      , ""
      , "Options listed in the defaults file are just that: defaults. You can"
      , "override any default on the command line."
      , ""
      , "Note that the use of `ALL` easily can have unpredicted consequences,"
      , "especially if commands in newer versions of darcs accepts flags that"
      , "they did not in previous versions. Only use safe flags with `ALL`."
      , ""
      , "For example, if your system clock is bizarre, you could instruct darcs to"
      , "always ignore the file modification times by adding the following line:"
      , ""
      , "    ALL ignore-times"
      , ""
      , "There are some options which are meant specifically for use in"
      , "`_darcs/prefs/defaults`. One of them is `--disable`. As the name"
      , "suggests, this option will disable every command that got it as"
      , "argument. So, if you are afraid that you could damage your repositories"
      , "by inadvertent use of a command like amend, add the following line:"
      , ""
      , "    amend disable"
      , ""
      , "A global defaults file can be created with the name"
      , "`.darcs/defaults` in your home directory. In case of conflicts,"
      , "the defaults for a specific repository take precedence."
      ])
    , ("boring", unlines
      [ "The `_darcs/prefs/boring` file may contain a list of regular expressions"
      , "describing files, such as object files, that you do not expect to add to"
      , "your project. A newly created repository has a boring file that includes"
      , "many common source control, backup, temporary, and compiled files."
      , ""
      , "You may want to have the boring file under version control. To do this"
      , "you can use darcs setpref to set the value 'boringfile' to the name of"
      , "your desired boring file (e.g. `darcs setpref boringfile .boring`, where"
      , "`.boring` is the repository path of a file that has been darcs added to"
      , "your repository). The boringfile preference overrides"
      , "`_darcs/prefs/boring`, so be sure to copy that file to the boringfile."
      , ""
      , "You can also set up a 'boring' regexps file in your home directory, named"
      , "`~/.darcs/boring`, which will be used with all of your darcs repositories."
      , ""
      , "Any file not already managed by darcs and whose repository path"
      , "matches any of the boring regular expressions is"
      , "considered boring. The boring file is used to filter the files provided"
      , "to darcs add, to allow you to use a simple `darcs add newdir newdir/*`"
      , "without accidentally adding a bunch of object files. It is also used"
      , "when the `--look-for-adds` flag is given to whatsnew or record. Note"
      , "that once a file has been added to darcs, it is not considered boring,"
      , "even if it matches the boring file filter."])
    , ("binaries", unlines
      [ "The `_darcs/prefs/binaries` file may contain a list of regular"
      , "expressions describing files that should be treated as binary files rather"
      , "than text files. Darcs automatically treats files containing characters"
      , "`^Z` or `NULL` within the first 4096 bytes as being binary files."
      , "You probably will want to have the binaries file under version control."
      , "To do this you can use `darcs setpref` to set the value 'binariesfile'"
      , "to the name of your desired binaries file"
      , "(e.g. `darcs setpref binariesfile ./.binaries`, where `.binaries` is a"
      , "file that has been darcs added to your repository). As with the boring"
      , "file, you can also set up a `~/.darcs/binaries` file if you like."])
    , ("defaultrepo", unlines
      [ "Contains the URL of the default remote repository used by commands `pull`,"
      , "`push`, `send` and `optimize relink`. Darcs edits this file automatically"
      , "or when the flag `--set-default` is used."])
    , ("sources", unlines
      [ "Besides the defaultrepo, darcs also keeps track of any other locations"
      , "used in commands for exchanging patches (e.g. push, pull, send)."
      , "These are subsequently used as alternatives from which to download"
      , "patches. The file contains lines such as:"
      , ""
      , "    cache:/home/droundy/.cache/darcs"
      , "    readonly:/home/otheruser/.cache/darcs"
      , "    repo:http://darcs.net"
      , ""
      , "The prefix `cache:` indicates that darcs can use this as a read-write"
      , "cache for patches, `read-only:` indicates a cache that is only"
      , "readable, and `repo:` denotes a (possibly remote) repository. The order"
      , "of the entries is immaterial: darcs will always try local paths before"
      , "remote ones, and only local ones will be used as potentially writable."
      , ""
      , "A global cache is enabled by default in your home directory under"
      , "`.cache/darcs` (older versions of darcs used `.darcs/cache` for this),"
      , "or `$XDG_CACHE_HOME/darcs` if the environment variable is set, see"
      , "https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html."
      , "The cache allows darcs to avoid re-downloading patches (for example, when"
      , "doing a second darcs clone of the same repository), and also allows darcs"
      , "to use hard links to reduce disk usage."
      , ""
      , "Note that the cache directory should reside on the same filesystem as"
      , "your repositories, so you may need to vary this. You can also use"
      , "multiple cache directories on different filesystems, if you have several"
      , "filesystems on which you use darcs."
      , ""
      , "While darcs automatically adds entries to `_darcs/prefs/sources`, it does"
      , "not currently remove them. If one or more of the entries aren't accessible"
      , "(e.g. because they resided on a removable media), then darcs will bugger"
      , "you with a hint, suggesting you remove those entries. This is done because"
      , "certain systems have extremely long timeouts associated with some remotely"
      , "accessible media (e.g. NFS over automounter on Linux), which can slow down"
      , "darcs operations considerably. On the other hand, when you clone a repo"
      , "with --lazy from a no longer accessible location, then the hint may give"
      , "you an idea where the patches could be found, so you can try to restore"
      , "access to them."
      ])
    , ("tmpdir", unlines
      [ "By default temporary directories are created in `/tmp`, or if that doesn't"
      , "exist, in `_darcs` (within the current repo).  This can be overridden by"
      , "specifying some other directory in the file `_darcs/prefs/tmpdir` or the"
      , "environment variable `$DARCS_TMPDIR` or `$TMPDIR`."])
    , ("prefs", unlines
      [ "Contains the preferences set by the command `darcs setprefs`."
      , "Do not edit manually."])
    ]
