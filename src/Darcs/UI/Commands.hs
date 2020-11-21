--  Copyright (C) 2002,2003,2005 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands
    ( CommandControl ( CommandData, HiddenCommand, GroupName )
    , DarcsCommand(..)
    , commandAlias
    , commandStub
    , commandOptions
    , commandAlloptions
    , withStdOpts
    , disambiguateCommands
    , CommandArgs(..)
    , getSubcommands
    , extractCommands
    , extractAllCommands
    , normalCommand
    , hiddenCommand
    , commandGroup
    , superName
    , nodefaults
    , putInfo
    , putVerbose
    , putWarning
    , putVerboseWarning
    , putFinished
    , abortRun
    , setEnvDarcsPatches
    , setEnvDarcsFiles
    , defaultRepo
    , amInHashedRepository
    , amInRepository
    , amNotInRepository
    , findRepository
    ) where

import Control.Monad ( when, unless )
import Data.List ( sort, isPrefixOf )
import System.Console.GetOpt ( OptDescr )
import System.IO ( stderr )
import System.IO.Error ( catchIOError )
import System.Environment ( setEnv )

import Darcs.Prelude

import Darcs.Patch ( listTouchedFiles )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Info ( toXml )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )

import qualified Darcs.Repository as R ( amInHashedRepository, amInRepository
                                       , amNotInRepository, findRepository )
import Darcs.Repository.Flags ( WorkRepo(..) )
import Darcs.Repository.Prefs ( defaultrepo )

import Darcs.UI.Options ( DarcsOption, DarcsOptDescr, (^), optDescr, odesc, parseFlags, (?) )
import Darcs.UI.Options.All
    ( StdCmdAction, stdCmdActions, debugging, UseCache, useCache, HooksConfig, hooks
    , Verbosity(..), DryRun(..), dryRun, newRepo, verbosity
    )

import Darcs.UI.Flags ( DarcsFlag, remoteRepos, workRepo, quiet, verbose )
import Darcs.UI.External ( viewDoc )
import Darcs.UI.PrintPatch ( showWithSummary )

import Darcs.Util.ByteString ( decodeLocale, packStringToUTF8 )
import Darcs.Util.Path ( AbsolutePath, anchorPath )
import Darcs.Util.Printer
    ( Doc, text, (<+>), ($$), ($+$), hsep, vcat
    , putDocLnWith, hPutDocLn, renderString
    )
import Darcs.Util.Printer.Color ( fancyPrinters, ePutDocLn )
import Darcs.Util.Progress
    ( debugMessage, beginTedious, endTedious, tediousSize, finishedOneIO )

extractCommands :: [CommandControl] -> [DarcsCommand]
extractCommands ccl = [ cmd | CommandData cmd <- ccl ]

extractHiddenCommands :: [CommandControl] -> [DarcsCommand]
extractHiddenCommands ccl = [ cmd | HiddenCommand cmd <- ccl ]

extractAllCommands :: [CommandControl] -> [DarcsCommand]
extractAllCommands ccl = concatMap flatten (extractCommands ccl ++ extractHiddenCommands ccl)
    where flatten c@(DarcsCommand {}) = [c]
          flatten c@(SuperCommand { commandSubCommands = scs }) = c : extractAllCommands scs

normalCommand :: DarcsCommand -> CommandControl
normalCommand c = CommandData c

hiddenCommand :: DarcsCommand -> CommandControl
hiddenCommand c = HiddenCommand c

commandGroup :: String -> CommandControl
commandGroup = GroupName

data CommandControl
  = CommandData DarcsCommand
  | HiddenCommand DarcsCommand
  | GroupName String

-- |A 'DarcsCommand' represents a command like add, record etc.
data DarcsCommand =
      DarcsCommand
          { commandProgramName -- programs that use libdarcs can change the name here
          , commandName :: String
          , commandHelp :: Doc
          , commandDescription :: String
          , commandExtraArgs :: Int
          , commandExtraArgHelp :: [String]
          , commandCommand :: -- First 'AbsolutePath' is the repository path,
                              -- second one is the path where darcs was executed.
                              (AbsolutePath, AbsolutePath)
                           -> [DarcsFlag] -> [String] -> IO ()
          , commandPrereq :: [DarcsFlag] -> IO (Either String ())
          , commandCompleteArgs :: (AbsolutePath, AbsolutePath)
                                -> [DarcsFlag] -> [String] -> IO [String]
          , commandArgdefaults :: [DarcsFlag] -> AbsolutePath -> [String]
                               -> IO [String]
          , commandBasicOptions :: [DarcsOptDescr DarcsFlag]
          , commandAdvancedOptions :: [DarcsOptDescr DarcsFlag]
          , commandDefaults :: [DarcsFlag]
          , commandCheckOptions :: [DarcsFlag] -> [String]
          }
    | SuperCommand
          { commandProgramName
          , commandName :: String
          , commandHelp :: Doc
          , commandDescription :: String
          , commandPrereq :: [DarcsFlag] -> IO (Either String ())
          , commandSubCommands :: [CommandControl]
          }

withStdOpts :: DarcsOption (Maybe StdCmdAction -> Verbosity -> b) c
            -> DarcsOption (UseCache -> HooksConfig -> Bool -> Bool -> Bool -> a) b
            -> DarcsOption a c
withStdOpts basicOpts advancedOpts =
  basicOpts ^ stdCmdActions ^ verbosity ^ advancedOpts ^ useCache ^ hooks ^ debugging

commandAlloptions :: DarcsCommand -> ([DarcsOptDescr DarcsFlag], [DarcsOptDescr DarcsFlag])
commandAlloptions DarcsCommand { commandBasicOptions = opts1
                               , commandAdvancedOptions = opts2 } =
    ( opts1 ++ odesc stdCmdActions
    , odesc verbosity ++ opts2 ++ odesc useCache ++ odesc hooks ++ odesc debugging
    )
commandAlloptions SuperCommand { } = (odesc stdCmdActions, [])

--  Obtain options suitable as input to System.Console.Getopt, including the
--  --disable option (which is not listed explicitly in the DarcsCommand
--  definitions).
commandOptions :: AbsolutePath -> DarcsCommand -> [OptDescr DarcsFlag]
commandOptions cwd = map (optDescr cwd) . uncurry (++) . commandAlloptions

nodefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
nodefaults _ _ = return

getSubcommands :: DarcsCommand -> [CommandControl]
getSubcommands c@(SuperCommand {}) = commandGroup "Subcommands:" : commandSubCommands c
getSubcommands _ = []

commandAlias :: String -> Maybe (DarcsCommand) -> DarcsCommand -> DarcsCommand
commandAlias alias msuper command =
  command
    { commandName = alias
    , commandDescription = "Alias for `" ++ prog ++ " " ++ cmdName ++ "'."
    , commandHelp =
        hsep
          [ "The"
          , "`" <> text prog <+> text alias <> "`"
          , "command is an alias for"
          , "`" <> text prog <+> text cmdName <> "`"
          ]
        $+$ "See description of `" <> text prog <+> text cmdName <> "` for details."
    }
  where
    prog = commandProgramName command
    cmdName = unwords . map commandName . maybe id (:) msuper $ [command]

commandStub :: String -> Doc -> String -> DarcsCommand -> DarcsCommand
commandStub n h d c = c { commandName = n
                        , commandHelp = h
                        , commandDescription = d
                        , commandCommand = \_ _ _ -> viewDoc h
                        }

superName :: Maybe (DarcsCommand) -> String
superName Nothing  = ""
superName (Just x) = commandName x ++ " "

data CommandArgs
  = CommandOnly DarcsCommand
  | SuperCommandOnly DarcsCommand
  | SuperCommandSub DarcsCommand DarcsCommand

-- Parses a darcs command line with potentially abbreviated commands
disambiguateCommands :: [CommandControl] -> String -> [String]
                     -> Either String (CommandArgs, [String])
disambiguateCommands allcs cmd args = do
    c <- extract cmd allcs
    case (getSubcommands c, args) of
        ([], _) -> return (CommandOnly c, args)
        (_, []) -> return (SuperCommandOnly c, args)
        (subcs, a : as) -> case extract a subcs of
                               Left _ -> return (SuperCommandOnly c, args)
                               Right sc -> return (SuperCommandSub c sc, as)

extract :: String -> [CommandControl] -> Either String DarcsCommand
extract cmd cs = case potentials of
    []  -> Left $ "No such command '" ++ cmd ++ "'\n"
    [c] -> Right c
    cs' -> Left $ unlines [ "Ambiguous command..."
                          , ""
                          , "The command '" ++ cmd ++ "' could mean one of:"
                          , unwords . sort . map commandName $ cs'
                          ]
  where
    potentials = [c | c <- extractCommands cs, cmd `isPrefixOf` commandName c]
                 ++ [h | h <- extractHiddenCommands cs, cmd == commandName h]

putVerbose :: [DarcsFlag] -> Doc -> IO ()
putVerbose flags = when (verbose flags) . putDocLnWith fancyPrinters

putInfo :: [DarcsFlag] -> Doc -> IO ()
putInfo flags = unless (quiet flags) . putDocLnWith fancyPrinters

putFinished :: [DarcsFlag] -> String -> IO ()
putFinished flags what =
    putInfo flags $ "Finished" <+> text what <> "."

putWarning :: [DarcsFlag] -> Doc -> IO ()
putWarning flags = unless (quiet flags) . ePutDocLn

putVerboseWarning :: [DarcsFlag] -> Doc -> IO ()
putVerboseWarning flags = when (verbose flags) . hPutDocLn stderr

abortRun :: [DarcsFlag] -> Doc -> IO ()
abortRun flags msg = if parseFlags dryRun flags == YesDryRun
                        then putInfo flags $ "NOTE:" <+> msg
                        else fail $ renderString msg

-- | Set the DARCS_PATCHES and DARCS_PATCHES_XML environment variables with
-- info about the given patches, for use in post-hooks.
setEnvDarcsPatches :: RepoPatch p => FL (PatchInfoAnd rt p) wX wY -> IO ()
setEnvDarcsPatches ps = do
    let k = "Defining set of chosen patches"
    let filepaths = map (anchorPath ".") (listTouchedFiles ps)
    debugMessage $ unlines ("setEnvDarcsPatches:" : filepaths)
    beginTedious k
    tediousSize k 3
    finishedOneIO k "DARCS_PATCHES"
    setEnvCautiously "DARCS_PATCHES" (renderString $ showWithSummary ps)
    finishedOneIO k "DARCS_PATCHES_XML"
    setEnvCautiously "DARCS_PATCHES_XML" . renderString $
        text "<patches>" $$
        vcat (mapFL (toXml . info) ps) $$
        text "</patches>"
    finishedOneIO k "DARCS_FILES"
    setEnvCautiously "DARCS_FILES" $ unlines filepaths
    endTedious k

-- | Set the DARCS_FILES environment variable to the files touched by the
-- given patch, one per line, for use in post-hooks.
setEnvDarcsFiles :: (PatchInspect p) => p wX wY -> IO ()
setEnvDarcsFiles ps = do
    let filepaths = map (anchorPath ".") (listTouchedFiles ps)
    setEnvCautiously "DARCS_FILES" $ unlines filepaths

-- | Set some environment variable to the given value, unless said value is
-- longer than 10K characters, in which case do nothing.
setEnvCautiously :: String -> String -> IO ()
setEnvCautiously e v
    | toobig (10 * 1024) v = return ()
    | otherwise =
        setEnv e v `catchIOError` (\_ -> setEnv e (decodeLocale (packStringToUTF8 v)))
  where
    -- note: not using (length v) because we want to be more lazy than that
    toobig :: Int -> [a] -> Bool
    toobig 0 _ = True
    toobig _ [] = False
    toobig n (_ : xs) = toobig (n - 1) xs

defaultRepo :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
defaultRepo fs = defaultrepo (remoteRepos ? fs)

amInHashedRepository :: [DarcsFlag] -> IO (Either String ())
amInHashedRepository fs = R.amInHashedRepository (workRepo fs)

amInRepository :: [DarcsFlag] -> IO (Either String ())
amInRepository fs = R.amInRepository (workRepo fs)

amNotInRepository :: [DarcsFlag] -> IO (Either String ())
amNotInRepository fs =
  R.amNotInRepository (maybe WorkRepoCurrentDir WorkRepoDir (newRepo ? fs))

findRepository :: [DarcsFlag] -> IO (Either String ())
findRepository fs = R.findRepository (workRepo fs)
