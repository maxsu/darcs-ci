--
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
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Util.Ssh
    (
      SshSettings(..)
    , defaultSsh
    , windows
    , copySSH
    , SSHCmd(..)
    , getSSH
    , environmentHelpSsh
    , environmentHelpScp
    , environmentHelpSshPort
    , transferModeHeader
    ) where

import Darcs.Prelude

import System.Environment ( getEnv )
import System.Exit ( ExitCode(..) )

import Control.Concurrent.MVar ( MVar, newMVar, withMVar, modifyMVar, modifyMVar_ )
import Control.Exception ( throwIO, catch, catchJust, SomeException )
import Control.Monad ( unless, (>=>) )

import qualified Data.ByteString as B (ByteString, hGet, writeFile )

import Data.Map ( Map, empty, insert, lookup )

import System.IO ( Handle, hSetBinaryMode, hPutStrLn, hGetLine, hFlush )
import System.IO.Unsafe ( unsafePerformIO )
import System.Process ( runInteractiveProcess, readProcessWithExitCode )

import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.URL ( SshFilePath, sshFilePathOf, sshUhost, sshRepo, sshFile )
import Darcs.Util.Exception ( prettyException, catchall )
import Darcs.Util.Exec ( readInteractiveProcess, ExecException(..), Redirect(AsIs) )
import Darcs.Util.Progress ( withoutProgress, debugMessage )

import qualified Darcs.Util.Ratified as Ratified ( hGetContents )

import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isPrefixOf )
import System.Info ( os )
import System.IO.Error ( ioeGetErrorType, isDoesNotExistErrorType )

import Darcs.Util.Global ( whenDebugMode )

windows :: Bool
windows = "mingw" `isPrefixOf` os

data SshSettings = SshSettings
    { ssh :: String
    , scp :: String
    , sftp :: String
    } deriving (Show, Eq)


_defaultSsh :: IORef SshSettings
_defaultSsh = unsafePerformIO $ newIORef =<< detectSsh
{-# NOINLINE _defaultSsh #-}

-- | Expected properties:
--
-- * only ever runs once in the lifetime of the program
-- * environment variables override all
-- * tries Putty first on Windows
-- * falls back to plain old ssh
detectSsh :: IO SshSettings
detectSsh = do
    whenDebugMode (putStrLn "Detecting SSH settings")
    vanilla <-  if windows
                  then do
                    plinkStr <- (snd3 <$> readProcessWithExitCode "plink" [] "")
                                  `catch` \(e :: SomeException) -> return (show e)
                    whenDebugMode $ putStrLn $
                        "SSH settings (plink): " ++
                        (concat . take 1 . lines $ plinkStr)
                    if "PuTTY" `isPrefixOf` plinkStr
                      then return (SshSettings "plink" "pscp -q" "psftp")
                      else return rawVanilla
                  else return rawVanilla
    settings <- SshSettings <$> fromEnv (ssh vanilla)  "DARCS_SSH"
                            <*> fromEnv (scp vanilla)  "DARCS_SCP"
                            <*> fromEnv (sftp vanilla) "DARCS_SFTP"
    whenDebugMode (putStrLn $ "SSH settings: " ++ show settings)
    return settings
  where
    snd3 (_, x, _) = x
    rawVanilla = SshSettings "ssh" "scp -q" "sftp"
    fromEnv :: String -> String -> IO String
    fromEnv d v = catchJust notFound
                            (getEnv v)
                            (const (return d))
    notFound e =  if isDoesNotExistErrorType (ioeGetErrorType e)
                  then Just ()
                  else Nothing


defaultSsh :: SshSettings
defaultSsh = unsafePerformIO $ readIORef _defaultSsh
{-# NOINLINE defaultSsh #-}

-- | A re-usable connection to a remote darcs in transfer-mode.
-- It contains the three standard handles.
data Connection = C
    { inp :: !Handle
    , out :: !Handle
    , err :: !Handle
    }

-- | Identifier (key) for a connection.
type RepoId = (String, String) -- (user@host,repodir)

-- | Global mutable variable that contains open connections,
-- identified by the repoid part of the ssh file name.
-- Only one thread can use a connection at a time, which is why
-- we stuff them behind their own 'MVar's.
--
-- We distinguish between a failed connection (represented by a
-- 'Nothing' entry in the map) and one that was never established
-- (the repoid is not in the map). Once a connection fails,
-- either when trying to establish it or during usage, it will not
-- be tried again.
sshConnections :: MVar (Map RepoId (Maybe (MVar Connection)))
sshConnections = unsafePerformIO $ newMVar empty
{-# NOINLINE sshConnections #-}

-- | Wait for an existing connection to become available or, if none
-- is available, try to create a new one and cache it.
getSshConnection :: String                       -- ^ remote darcs command
                 -> SshFilePath                  -- ^ destination
                 -> IO (Maybe (MVar Connection)) -- ^ wrapper for the action
getSshConnection rdarcs sshfp = modifyMVar sshConnections $ \cmap -> do
  let key = repoid sshfp
  case lookup key cmap of
    Nothing -> do
      -- we have not yet tried with this key, do it now
      mc <- newSshConnection rdarcs sshfp
      case mc of
        Nothing ->
          -- failed, remember it, so we don't try again
          return (insert key Nothing cmap, Nothing)
        Just c -> do
          -- success, remember and use
          v <- newMVar c
          return (insert key (Just v) cmap, Just v)
    Just Nothing ->
      -- we have tried to connect before, don't do it again
      return (cmap, Nothing)
    Just (Just v) ->
      -- we do have a connection, return an action that
      -- waits until it is available
      return (cmap, Just v)

-- | Try to create a new ssh connection to a remote darcs that runs the
-- transfer-mode command. This is tried only once per repoid.
newSshConnection :: String -> SshFilePath -> IO (Maybe Connection)
newSshConnection rdarcs sshfp = do
  (sshcmd,sshargs_) <- getSSH SSH
  debugMessage $ "Starting new ssh connection to " ++ sshUhost sshfp
  let sshargs = sshargs_ ++ ["--", sshUhost sshfp, rdarcs,
                             "transfer-mode", "--repodir", sshRepo sshfp]
  debugMessage $ "Exec: " ++ showCommandLine (sshcmd:sshargs)
  (i,o,e,_) <- runInteractiveProcess sshcmd sshargs Nothing Nothing
  do
    hSetBinaryMode i True
    hSetBinaryMode o True
    l <- hGetLine o
    unless (l == transferModeHeader) $
      fail "Couldn't start darcs transfer-mode on server"
    return $ Just C { inp = i, out = o, err = e }
    `catchNonSignal` \exn -> do
      debugMessage $ "Failed to start ssh connection: " ++ prettyException exn
      debugMessage $ unlines
                    [ "NOTE: the server may be running a version of darcs prior to 2.0.0."
                    , ""
                    , "Installing darcs 2 on the server will speed up ssh-based commands."
                    ]
      return Nothing

-- | Mark any connection associated with the given ssh file path
-- as failed, so it won't be tried again.
dropSshConnection :: RepoId -> IO ()
dropSshConnection key = do
  debugMessage $ "Dropping ssh failed connection to " ++ fst key ++ ":" ++ snd key
  modifyMVar_ sshConnections (return . insert key Nothing)

repoid :: SshFilePath -> RepoId
repoid sshfp = (sshUhost sshfp, sshRepo sshfp)

grabSSH :: SshFilePath -> Connection -> IO B.ByteString
grabSSH src c = do
  debugMessage $ "grabSSH src=" ++ sshFilePathOf src
  let failwith e = do dropSshConnection (repoid src)
                        -- hGetContents is ok here because we're
                        -- only grabbing stderr, and we're also
                        -- about to throw the contents.
                      eee <- Ratified.hGetContents (err c)
                      fail $ e ++ " grabbing ssh file " ++
                        sshFilePathOf src ++"\n" ++ eee
      file = sshFile src
  hPutStrLn (inp c) $ "get " ++ file
  hFlush (inp c)
  l2 <- hGetLine (out c)
  if l2 == "got "++file
    then do showlen <- hGetLine (out c)
            case reads showlen of
              [(len,"")] -> B.hGet (out c) len
              _ -> failwith "Couldn't get length"
    else if l2 == "error "++file
         then do e <- hGetLine (out c)
                 case reads e of
                   (msg,_):_ -> fail $ "Error reading file remotely:\n"++msg
                   [] -> failwith "An error occurred"
         else failwith "Error"

copySSH :: String -> SshFilePath -> FilePath -> IO ()
copySSH rdarcs src dest = do
  debugMessage $ "copySSH file: " ++ sshFilePathOf src
  -- TODO why do we disable progress reporting here?
  withoutProgress $ do
    mc <- getSshConnection rdarcs src
    case mc of
      Just v -> withMVar v (grabSSH src >=> B.writeFile dest)
      Nothing -> do
        -- remote 'darcs transfer-mode' does not work => use scp
        let u = escape_dollar $ sshFilePathOf src
        (scpcmd, args) <- getSSH SCP
        let scp_args = filter (/="-q") args ++ ["--", u, dest]
        debugMessage $ "Exec: " ++ showCommandLine (scpcmd:scp_args)
        (r, scp_err) <- readInteractiveProcess scpcmd scp_args
        unless (r == ExitSuccess) $
          throwIO $ ExecException scpcmd scp_args (AsIs,AsIs,AsIs) scp_err
  where
    -- '$' in filenames is troublesome for scp, for some reason.
    escape_dollar :: String -> String
    escape_dollar = concatMap tr
      where
        tr '$' = "\\$"
        tr c = [c]

-- | Show a command and its arguments for debug messages.
showCommandLine :: [String] -> String
showCommandLine = unwords . map show

transferModeHeader :: String
transferModeHeader = "Hello user, I am darcs transfer mode"

-- ---------------------------------------------------------------------
-- older ssh helper functions
-- ---------------------------------------------------------------------

data SSHCmd = SSH
            | SCP
            | SFTP


fromSshCmd :: SshSettings
           -> SSHCmd
           -> String
fromSshCmd s SSH  = ssh s
fromSshCmd s SCP  = scp s
fromSshCmd s SFTP = sftp s

-- | Return the command and arguments needed to run an ssh command
--   First try the appropriate darcs environment variable and SSH_PORT
--   defaulting to "ssh" and no specified port.
getSSH :: SSHCmd
       -> IO (String, [String])
getSSH cmd = do
    port <- (portFlag cmd `fmap` getEnv "SSH_PORT") `catchall` return []
    let (sshcmd, ssh_args) = breakCommand command
    return (sshcmd, ssh_args ++ port)
  where
    command = fromSshCmd defaultSsh cmd
    portFlag SSH  x = ["-p", x]
    portFlag SCP  x = ["-P", x]
    portFlag SFTP x = ["-oPort=" ++ x]
    breakCommand s =
      case words s of
        (arg0:args) -> (arg0, args)
        [] -> (s, [])

environmentHelpSsh :: ([String], [String])
environmentHelpSsh = (["DARCS_SSH"], [
    "Repositories of the form [user@]host:[dir] are taken to be remote",
    "repositories, which Darcs accesses with the external program ssh(1).",
    "",
    "The environment variable $DARCS_SSH can be used to specify an",
    "alternative SSH client.  Arguments may be included, separated by",
    "whitespace.  The value is not interpreted by a shell, so shell",
    "constructs cannot be used; in particular, it is not possible for the",
    "program name to contain whitespace by using quoting or escaping."])


environmentHelpScp :: ([String], [String])
environmentHelpScp = (["DARCS_SCP", "DARCS_SFTP"], [
    "When reading from a remote repository, Darcs will attempt to run",
    "`darcs transfer-mode` on the remote host.  This will fail if the",
    "remote host only has Darcs 1 installed, doesn't have Darcs installed",
    "at all, or only allows SFTP.",
    "",
    "If transfer-mode fails, Darcs will fall back on scp(1) and sftp(1).",
    "The commands invoked can be customized with the environment variables",
    "$DARCS_SCP and $DARCS_SFTP respectively, which behave like $DARCS_SSH.",
    "If the remote end allows only sftp, try setting DARCS_SCP=sftp."])


environmentHelpSshPort :: ([String], [String])
environmentHelpSshPort = (["SSH_PORT"], [
    "If this environment variable is set, it will be used as the port",
    "number for all SSH calls made by Darcs (when accessing remote",
    "repositories over SSH).  This is useful if your SSH server does not",
    "run on the default port, and your SSH client does not support",
    "ssh_config(5).  OpenSSH users will probably prefer to put something",
    "like `Host *.example.net Port 443` into their ~/.ssh/config file."])
