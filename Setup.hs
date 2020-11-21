-- copyright (c) 2008 Duncan Coutts
-- portions copyright (c) 2008 David Roundy
-- portions copyright (c) 2007-2009 Judah Jacobson
{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.PackageDescription ( PackageDescription )
import Distribution.Package ( packageVersion )
import Distribution.Version( Version )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), absoluteInstallDirs )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.Setup
    (buildVerbosity, copyDest, copyVerbosity, fromFlag,
     haddockVerbosity, installVerbosity, replVerbosity )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.Utils
    (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout,
     rewriteFileEx)
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Text ( display )

import Control.Monad ( unless, when, void )
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.IO ( openFile, IOMode(..) )
import System.Process (callProcess, runProcess)
import Data.List( isInfixOf )
import System.FilePath ( (</>) )

import qualified Control.Exception as Exception

catchAny :: IO a -> (Exception.SomeException -> IO a) -> IO a
catchAny f h = Exception.catch f (\e -> h (e :: Exception.SomeException))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {

  buildHook = \ pkg lbi hooks flags ->
              let verb = fromFlag $ buildVerbosity flags
               in commonBuildHook buildHook pkg lbi hooks verb >>= ($ flags),

  haddockHook = \ pkg lbi hooks flags ->
                let verb = fromFlag $ haddockVerbosity flags
                 in commonBuildHook haddockHook pkg lbi hooks verb >>= ($ flags) ,
  replHook = \pkg lbi hooks flags args ->
                let verb = fromFlag $ replVerbosity flags
                 in commonBuildHook replHook pkg lbi hooks verb >>= (\f -> f flags args) ,
  postBuild = \ _ _ _ lbi -> buildManpage lbi,
  postCopy = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags),
  postInst = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest

}

-- | For @./Setup build@ and @./Setup haddock@, do some unusual
-- things, then invoke the base behaviour ("simple hook").
commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a)
                -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  versionInfoExists <-
    (&&) <$> doesFileExist "release/distributed-context"
         <*> doesFileExist "release/distributed-version"
  unless versionInfoExists $
    callProcess "runghc" ["./release/gen-version-info.hs"]
  (version, state) <- determineVersion verbosity pkg
  generateVersionModule verbosity lbi version state
  return $ runHook simpleUserHooks pkg lbi hooks

-- ---------------------------------------------------------------------
-- man page
-- ---------------------------------------------------------------------

buildManpage :: LocalBuildInfo -> IO ()
buildManpage lbi = do
  have_darcs_exe_dir <- doesDirectoryExist (buildDir lbi </> "darcs")
  when have_darcs_exe_dir $ do
    let darcs = buildDir lbi </> "darcs/darcs"
        manpage = buildDir lbi </> "darcs/darcs.1"
    manpageHandle <- openFile manpage WriteMode
    void $ runProcess darcs ["help","manpage"]
            Nothing Nothing Nothing (Just manpageHandle) Nothing

installManpage :: PackageDescription -> LocalBuildInfo -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy = do
  have_manpage <- doesFileExist (buildDir lbi </> "darcs" </> "darcs.1")
  when have_manpage $
    copyFiles
      verbosity
      (mandir (absoluteInstallDirs pkg lbi copy) </> "man1")
      [(buildDir lbi </> "darcs", "darcs.1")]

-- ---------------------------------------------------------------------
-- version module
-- ---------------------------------------------------------------------

determineVersion :: Verbosity -> PackageDescription -> IO (String, String)
determineVersion verbosity pkg = do
  let darcsVersion = packageVersion pkg
  numPatches <- versionPatches verbosity darcsVersion
  return (display darcsVersion, versionStateString numPatches)

  where
    versionStateString :: Maybe Int -> String
    versionStateString Nothing  = "unknown"
    versionStateString (Just 0) = "release"
    versionStateString (Just 1) = "+ 1 patch"
    versionStateString (Just n) = "+ " ++ show n ++ " patches"

versionPatches :: Verbosity -> Version -> IO (Maybe Int)
versionPatches verbosity darcsVersion = do
  numPatchesDarcs <- do
      out <- rawSystemStdout verbosity "darcs"
               ["log", "-a", "--from-tag", display darcsVersion, "--count"]
      case reads out of
        ((n,_):_) -> return $ Just ((n :: Int) - 1)
        _         -> return Nothing
    `catchAny` \_ -> return Nothing
  numPatchesDist <- parseFile "release/distributed-version"
  return $ case (numPatchesDarcs, numPatchesDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing

generateVersionModule :: Verbosity -> LocalBuildInfo -> String -> String -> IO ()
generateVersionModule verbosity lbi version state = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  ctx <- context verbosity
  hash <- weakhash verbosity
  rewriteFileEx silent (dir </> "Version.hs") $ unlines
    ["module Version where"
    ,"import Darcs.Prelude"
    ,"version, weakhash, context :: String"
    ,"version = \"" ++ version ++ " (" ++ state ++ ")\""
    ,"weakhash = " ++ case hash of
                        Just x -> show x
                        Nothing -> show "not available"
    ,"context = " ++ case ctx of
                       Just x -> show x
                       Nothing -> show "context not available"
    ]

weakhash :: Verbosity -> IO (Maybe String)
weakhash verbosity = do
  inrepo <- doesDirectoryExist "_darcs"
  unless inrepo $ fail "Not a repository."
  out <- rawSystemStdout verbosity "darcs" ["show", "repo"]
  let line = filter ("Weak Hash:" `isInfixOf`) $ lines out
  return $ case (length line) of
                0 -> Nothing
                _ -> Just $ last $ words $ head line
 `catchAny` \_ -> return Nothing

context :: Verbosity -> IO (Maybe String)
context verbosity =
   do
      inrepo <- doesDirectoryExist "_darcs"
      unless inrepo $ fail "Not a repository."
      out <- rawSystemStdout verbosity "darcs" ["log", "-a", "--context"]
      return $ Just out
   `catchAny` \_ -> parseFile "release/distributed-context"

parseFile :: Read a => FilePath -> IO (Maybe a)
parseFile f = do
  exist <- doesFileExist f
  if exist
    then do
      content <- readFile f
      case reads content of
        ((s, _):_) -> return s
        _ -> return Nothing
    else return Nothing
