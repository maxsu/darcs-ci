import Control.Monad ( unless )
import System.Directory ( doesDirectoryExist )
import System.Environment ( getArgs )
import System.Process ( readProcess )

main :: IO ()
main = do
  args <- getArgs
  let versionTag = case args of [] -> "."; arg:_ -> arg
  generateVersionInfo versionTag

versionPatches :: String -> IO (Maybe Int)
versionPatches version = do
  out <- readProcess "darcs" ["log", "-a", "--from-tag", version, "--count"] ""
  case reads out of
    ((n, _):_) -> return $ Just ((n :: Int) - 1)
    _ -> return Nothing

generateVersionInfo :: String -> IO ()
generateVersionInfo version = do
  ctx <- context
  writeFile "release/distributed-context" $ show ctx
  patches <- versionPatches version
  writeFile "release/distributed-version" $ show patches

context :: IO (Maybe String)
context = do
  inrepo <- doesDirectoryExist "_darcs"
  if inrepo
    then do
      out <- readProcess "darcs" ["log", "-a", "--context"] ""
      return $ Just out
    else return Nothing
