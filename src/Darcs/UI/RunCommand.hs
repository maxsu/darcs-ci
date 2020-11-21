-- Copyright (C) 2002,2003,2005 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
-- | This is the actual heavy lifter code, which is responsible for parsing the
-- arguments and then running the command itself.
module Darcs.UI.RunCommand
  ( runTheCommand
  , runWithHooks -- exported for darcsden
  ) where

import Darcs.Prelude

import Control.Monad ( unless, when )
import System.Console.GetOpt( ArgOrder( Permute, RequireOrder ),
                              OptDescr( Option ),
                              getOpt )
import System.Exit ( ExitCode ( ExitSuccess ), exitWith )

import Darcs.UI.Options ( (^), odesc, oparse, parseFlags, optDescr, (?) )
import Darcs.UI.Options.All
    ( stdCmdActions, StdCmdAction(..)
    , debugging, verbosity, Verbosity(..), network, NetworkOptions(..)
    , HooksConfig(..), hooks )

import Darcs.UI.Defaults ( applyDefaults )
import Darcs.UI.External ( viewDoc )
import Darcs.UI.Flags ( DarcsFlag, matchAny, fixRemoteRepos, withNewRepo )
import Darcs.UI.Commands
    ( CommandArgs( CommandOnly, SuperCommandOnly, SuperCommandSub )
    , CommandControl
    , DarcsCommand
    , commandName
    , commandCommand
    , commandPrereq
    , commandExtraArgHelp
    , commandExtraArgs
    , commandArgdefaults
    , commandCompleteArgs
    , commandOptions
    , commandName
    , disambiguateCommands
    , getSubcommands
    , extractCommands
    , superName
    )
import Darcs.UI.Commands.GZCRCs ( doCRCWarnings )
import Darcs.UI.Commands.Clone ( makeRepoName, cloneToSSH )
import Darcs.UI.Usage
    ( getCommandHelp
    , getCommandMiniHelp
    , subusage
    )

import Darcs.Patch.Match ( checkMatchSyntax )
import Darcs.Repository.Prefs ( getGlobal, getPreflist )
import Darcs.Repository.Test ( runPosthook, runPrehook )
import Darcs.Util.AtExit ( atexit )
import Darcs.Util.Download ( setDebugHTTP, disableHTTPPipelining )
import Darcs.Util.Exception ( die )
import Darcs.Util.Global ( setDebugMode, setTimingsMode )
import Darcs.Util.Path ( AbsolutePath, getCurrentDirectory, toPath, ioAbsoluteOrRemote, makeAbsolute )
import Darcs.Util.Printer ( (<+>), ($+$), renderString, text, vcat )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Progress ( setProgressMode )

runTheCommand :: [CommandControl] -> String -> [String] -> IO ()
runTheCommand commandControlList cmd args =
  either die rtc $ disambiguateCommands commandControlList cmd args
 where
  rtc (CommandOnly c,       as) = runCommand Nothing c as
  rtc (SuperCommandOnly c,  as) = runRawSupercommand c as
  rtc (SuperCommandSub c s, as) = runCommand (Just c) s as

runCommand :: Maybe DarcsCommand -> DarcsCommand -> [String] -> IO ()
runCommand _ _ args -- Check for "dangerous" typoes...
    | "-all" `elem` args = -- -all indicates --all --look-for-adds!
        die "Are you sure you didn't mean --all rather than -all?"
runCommand msuper cmd args = do
  old_wd <- getCurrentDirectory
  let options = commandOptions old_wd cmd
  case fixupMsgs $ getOpt Permute options args of
    (cmdline_flags,orig_extra,getopt_errs) -> do
      -- FIXME This code is highly order-dependent because of hidden state: the
      -- current directory. Like almost all Repository functions, getGlobal and
      -- getPreflist assume that the cwd is the base of our work repo (if any).
      -- This is supposed to be ensured by commandPrereq. Which means we must
      -- first call commandPrereq, then getGlobal and getPreflist, and then we
      -- must use the (saved) original working directory to resolve possibly
      -- relative paths to absolute paths.
      prereq_errors <- commandPrereq cmd cmdline_flags
      -- we must get the cwd again because commandPrereq has the side-effect of changing it.
      new_wd <- getCurrentDirectory
      user_defs <- getGlobal   "defaults"
      repo_defs <- getPreflist "defaults"
      let (flags,flag_errors) =
            applyDefaults (fmap commandName msuper) cmd old_wd user_defs repo_defs cmdline_flags
      case parseFlags stdCmdActions flags of
        Just Help -> viewDoc $ getCommandHelp msuper cmd
        Just ListOptions -> do
          setProgressMode False
          possible_args <- commandCompleteArgs cmd (new_wd, old_wd) flags orig_extra
          mapM_ putStrLn $ optionList options ++ possible_args
        Just Disable ->
          die $ "Command "++commandName cmd++" disabled with --disable option!"
        Nothing -> case prereq_errors of
          Left complaint -> die $
            "Unable to '" ++ "darcs " ++ superName msuper ++ commandName cmd ++
            "' here:\n" ++ complaint
          Right () -> do
            ePutDocLn $ vcat $ map text $ getopt_errs ++ flag_errors
            extra <- commandArgdefaults cmd flags old_wd orig_extra
            case extraArgumentsError extra cmd msuper of
              Nothing     -> runWithHooks cmd (new_wd, old_wd) flags extra
              Just msg    -> die msg

fixupMsgs :: (a, b, [String]) -> (a, b, [String])
fixupMsgs (fs,as,es) = (fs,as,map (("command line: "++).chompTrailingNewline) es)
  where
    chompTrailingNewline "" = ""
    chompTrailingNewline s = if last s == '\n' then init s else s

runWithHooks :: DarcsCommand
             -> (AbsolutePath, AbsolutePath)
             -> [DarcsFlag] -> [String] -> IO ()
runWithHooks cmd (new_wd, old_wd) flags extra = do
   checkMatchSyntax $ matchAny ? flags
   -- set any global variables
   oparse (verbosity ^ debugging ^ network) setGlobalVariables flags
   -- actually run the command and its hooks
   let hooksCfg = parseFlags hooks flags
   let verb = parseFlags verbosity flags
   preHookExitCode <- runPrehook (pre hooksCfg) verb new_wd
   if preHookExitCode /= ExitSuccess
      then exitWith preHookExitCode
      else do fixedFlags <- fixRemoteRepos old_wd flags
              phDir <- getPosthookDir new_wd cmd fixedFlags extra
              commandCommand cmd (new_wd, old_wd) fixedFlags extra
              postHookExitCode <- runPosthook (post hooksCfg) verb phDir
              exitWith postHookExitCode

setGlobalVariables :: Verbosity -> Bool -> Bool -> Bool -> NetworkOptions -> IO ()
setGlobalVariables verb debug debugHttp timings net = do
  when timings setTimingsMode
  when debug setDebugMode
  when debugHttp setDebugHTTP
  when (verb == Quiet) $ setProgressMode False
  when (noHttpPipelining net) disableHTTPPipelining
  unless (verb == Quiet) $ atexit $ doCRCWarnings (verb == Verbose)

-- | Returns the working directory for the posthook. For most commands, the
-- first parameter is returned. For the \'get\' command, the path of the newly
-- created repository is returned if it is not an ssh url.
getPosthookDir :: AbsolutePath -> DarcsCommand -> [DarcsFlag] -> [String] -> IO AbsolutePath
getPosthookDir new_wd cmd flags extra | commandName cmd `elem` ["get","clone"] = do
    case extra of
      [inrepodir, outname] -> getPosthookDir new_wd cmd (withNewRepo outname flags) [inrepodir]
      [inrepodir] ->
        case cloneToSSH flags of
         Nothing -> do
          repodir <- toPath <$> ioAbsoluteOrRemote inrepodir
          newRepo <- makeRepoName False flags repodir
          return $ makeAbsolute new_wd newRepo
         _ -> return new_wd
      _ -> die "You must provide 'clone' with either one or two arguments."
getPosthookDir new_wd _ _ _ = return new_wd


-- | Checks if the number of extra arguments matches the number of extra
-- arguments supported by the command as specified in `commandExtraArgs`.
-- Extra arguments are arguments that follow the command but aren't
-- considered a flag. In `darcs push xyz`, xyz would be an extra argument.
extraArgumentsError :: [String]             -- extra commands provided by user
                    -> DarcsCommand
                    -> Maybe DarcsCommand
                    -> Maybe String
extraArgumentsError extra cmd msuper
    | extraArgsCmd < 0 = Nothing
    | extraArgsInput > extraArgsCmd = Just badArg
    | extraArgsInput < extraArgsCmd = Just missingArg
    | otherwise = Nothing
        where
            extraArgsInput = length extra
            extraArgsCmd = commandExtraArgs cmd
            badArg     = "Bad argument: `" ++ unwords extra ++
                         "'\n" ++ getCommandMiniHelp msuper cmd
            missingArg = "Missing argument:  " ++ nthArg (length extra + 1) ++
                         "\n" ++ getCommandMiniHelp msuper cmd
            nthArg n       = nthOf n (commandExtraArgHelp cmd)
            nthOf 1 (h:_)  = h
            nthOf n (_:hs) = nthOf (n-1) hs
            nthOf _ []     = "UNDOCUMENTED"

optionList :: [OptDescr DarcsFlag] -> [String]
optionList = concatMap names
  where
    names (Option sos los _ desc) =
      map (short desc) sos ++ map (long desc) los
    short d o = '-' : o : ";" ++ d
    long d o = "--" ++ o ++ ";" ++ d

runRawSupercommand :: DarcsCommand -> [String] -> IO ()
runRawSupercommand super [] =
  die $ renderString $
    "Command '" <> text (commandName super) <> "' requires a subcommand!"
    $+$
    subusage super
runRawSupercommand super args = do
  cwd <- getCurrentDirectory
  case fixupMsgs $ getOpt RequireOrder (map (optDescr cwd) (odesc stdCmdActions)) args of
    -- note: we do not apply defaults here
    (flags,_,getopt_errs) -> case parseFlags stdCmdActions flags of
      Just Help ->
        viewDoc $ getCommandHelp Nothing super
      Just ListOptions -> do
        putStrLn "--help"
        mapM_ (putStrLn . commandName) (extractCommands $ getSubcommands super)
      Just Disable -> do
        die $ renderString $
          "Command" <+> text (commandName super) <+> "disabled with --disable option!"
      Nothing ->
        die $ renderString $
          case getopt_errs of
            [] -> text "Invalid subcommand!" $+$ subusage super
            _ -> vcat (map text getopt_errs)
