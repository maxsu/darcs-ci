--  Copyright (C) 2002-2004 David Roundy
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
module Darcs.UI.Commands.Help
    ( helpCmd
    , commandControlList
    , printVersion
    , listAvailableCommands
    ) where

import Darcs.Prelude

import Control.Arrow ( (***) )
import Data.Char ( isAlphaNum, toLower, toUpper )
import Data.Either ( partitionEithers )
import Data.List ( groupBy, intercalate, lookup, nub )
import System.Exit ( exitSuccess )
import Version ( version )

import Darcs.Patch.Match ( helpOnMatchers )
import Darcs.Repository.Prefs ( environmentHelpHome, prefsFilesHelp )

import Darcs.UI.Commands
    ( CommandArgs(..)
    , CommandControl(..)
    , DarcsCommand(..)
    , commandName
    , disambiguateCommands
    , extractCommands
    , getSubcommands
    , nodefaults
    , normalCommand
    )
import Darcs.UI.External ( viewDoc )
import Darcs.UI.Flags ( DarcsFlag, environmentHelpEmail, environmentHelpSendmail )
import Darcs.UI.Options ( defaultFlags, ocheck, oid )
import Darcs.UI.Options.Markdown ( optionsMarkdown )
import qualified Darcs.UI.TheCommands as TheCommands
import Darcs.UI.Usage ( getCommandHelp, getSuperCommandHelp, subusage, usage )

import Darcs.Util.Download ( environmentHelpProxy, environmentHelpProxyPassword )
import Darcs.Util.English ( andClauses )
import Darcs.Util.File ( withCurrentDirectory )
import Darcs.Util.Lock
    ( environmentHelpKeepTmpdir
    , environmentHelpLocks
    , environmentHelpTmpdir
    )
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer
    ( Doc
    , empty
    , formatWords
    , quoted
    , renderString
    , text
    , vcat
    , vsep
    , ($$)
    , ($+$)
    , (<+>)
    )
import Darcs.Util.Printer.Color
    ( environmentHelpColor
    , environmentHelpEscape
    , environmentHelpEscapeWhite
    )
import Darcs.Util.Ssh
    ( environmentHelpScp
    , environmentHelpSsh
    , environmentHelpSshPort
    )
import Darcs.Util.Workaround ( getCurrentDirectory )


helpDescription :: String
helpDescription = "Display help about darcs and darcs commands."

helpHelp :: Doc
helpHelp = formatWords
  [ "Without arguments, `darcs help` prints a categorized list of darcs"
  , "commands and a short description of each one.  With an extra argument,"
  , "`darcs help foo` prints detailed help about the darcs command foo."
  ]

-- | Starting from a list of 'CommandControl's, unwrap one level
-- to get a list of command names together with their subcommands.
unwrapTree :: [CommandControl] -> [(String, [CommandControl])]
unwrapTree cs = [ (commandName c, getSubcommands c) | CommandData c <- cs ]

-- | Given a list of (normal) arguments to the help command, produce a list
-- of possible completions for the next (normal) argument.
completeArgs :: [String] -> [String]
completeArgs [] = map fst (unwrapTree commandControlList) ++ extraArgs where
  extraArgs = [ "patterns", "preferences", "environment", "manpage", "markdown" ]
completeArgs (arg:args) = exploreTree arg args commandControlList where
  exploreTree cmd cmds cs =
    case lookup cmd (unwrapTree cs) of
      Nothing -> []
      Just cs' -> case cmds of
        [] -> map fst (unwrapTree cs')
        sub:cmds' -> exploreTree sub cmds' cs'

help :: DarcsCommand
help = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "help"
    , commandHelp = helpHelp
    , commandDescription = helpDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DARCS_COMMAND> [DARCS_SUBCOMMAND]]  "]
    , commandCommand = \ x y z -> helpCmd x y z >> exitSuccess
    , commandPrereq = \_ -> return $ Right ()
    , commandCompleteArgs = \_ _ -> return . completeArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = []
    , commandBasicOptions = []
    , commandDefaults = defaultFlags oid
    , commandCheckOptions = ocheck oid
    }

helpCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
helpCmd _ _ ["manpage"] = viewDoc manpage
helpCmd _ _ ["markdown"] = viewDoc $ vcat $ map text markdownLines
helpCmd _ _ ["patterns"] = viewDoc $ vcat $ map text helpOnMatchers
helpCmd _ _ ["preferences"] =
    viewDoc $ header $+$ vcat (map render prefsFilesHelp)
  where
    header = "Preference Files" $$
             "================"
    render (f, h) =
      let item = "_darcs/prefs/" ++ f in
        text item $$ text (replicate (length item) '-') $$ text h
helpCmd _ _ ("environment":vs_) =
    viewDoc $ vsep (header : map render known) $+$ footer
  where
    header | null known = empty
           | otherwise = "Environment Variables" $$
                         "====================="

    footer | null unknown = empty
           | otherwise = text ("Unknown environment variables: "
                               ++ intercalate ", " unknown)

    render (ks, ds) = text (andClauses ks ++ ":") $$
                      vcat [ text ("  " ++ d) | d <- ds ]

    (unknown, known) = case map (map toUpper) vs_ of
                           [] -> ([], environmentHelp)
                           vs -> (nub *** (nub . concat)) . partitionEithers $
                                     map doLookup vs

    -- v is not known if it doesn't appear in the list of aliases of any
    -- of the environment var help descriptions.
    doLookup v = case filter ((v `elem`) . fst) environmentHelp of
                     [] -> Left v
                     es -> Right es

helpCmd _ _ [] = viewDoc $ usage commandControlList

helpCmd _ _ (cmd:args) =
    case disambiguateCommands commandControlList cmd args of
         Left err -> fail err
         Right (cmds,as) ->
             let msg = case cmds of
                         CommandOnly c       -> getCommandHelp Nothing c
                         SuperCommandOnly c  ->
                          if null as then
                            getSuperCommandHelp c
                          else
                            "Invalid subcommand!" $+$ subusage c
                         SuperCommandSub c s -> getCommandHelp (Just c) s
             in viewDoc $ msg

listAvailableCommands :: IO ()
listAvailableCommands =
    do here <- getCurrentDirectory
       is_valid <- mapM
                   (\c -> withCurrentDirectory here $ commandPrereq c [])
                   (extractCommands commandControlList)
       putStr $ unlines $ map (commandName . fst) $
                filter (isRight.snd) $
                zip (extractCommands commandControlList) is_valid
       putStrLn "--help"
       putStrLn "--version"
       putStrLn "--exact-version"
    where isRight (Right _) = True
          isRight _ = False

printVersion :: IO ()
printVersion = putStrLn $ "darcs version " ++ version

-- avoiding a module import cycle between Help and TheCommands
commandControlList :: [CommandControl]
commandControlList =
  normalCommand help : TheCommands.commandControlList

-- FIXME: the "grouping" comments below should made subsections in the
-- manpage, as we already do for DarcsCommand groups. --twb, 2009

-- | Help on each environment variable in which Darcs is interested.
environmentHelp :: [([String], [String])]
environmentHelp = [
 -- General-purpose
 environmentHelpHome,
 environmentHelpEditor,
 environmentHelpPager,
 environmentHelpColor,
 environmentHelpEscapeWhite,
 environmentHelpEscape,
 environmentHelpTmpdir,
 environmentHelpKeepTmpdir,
 environmentHelpEmail,
 environmentHelpSendmail,
 environmentHelpLocks,
 -- Remote Repositories
 environmentHelpSsh,
 environmentHelpScp,
 environmentHelpSshPort,
 environmentHelpProxy,
 environmentHelpProxyPassword,
 environmentHelpTimeout]

-- | This function is responsible for emitting a darcs "man-page", a
-- reference document used widely on Unix-like systems.  Manpages are
-- primarily used as a quick reference, or "memory jogger", so the
-- output should be terser than the user manual.
--
-- Before modifying the output, please be sure to read the man(7) and
-- man-pages(7) manpages, as these respectively describe the relevant
-- syntax and conventions.
manpage :: Doc
manpage = vcat [
 ".TH DARCS 1" <+> quoted version,
 ".SH NAME",
 "darcs \\- an advanced revision control system",
 ".SH SYNOPSIS",
 ".B darcs", ".I command", ".RI < arguments |[ options ]>...",
 "",
 "Where the", ".I commands", "and their respective", ".I arguments", "are",
 "",
 synopsis,
 ".SH DESCRIPTION",
 description,
 ".SH OPTIONS",
 "Different options are accepted by different Darcs commands.",
 "Each command's most important options are listed in the",
 ".B COMMANDS",
 "section.  For a full list of all options accepted by",
 "a particular command, run `darcs", ".I command", "\\-\\-help'.",
 ".SS " <> vcat (map text helpOnMatchers),
 ".SH COMMANDS",
 commands,
 ".SH ENVIRONMENT",
 environment,
 ".SH FILES",
 prefFiles,
 ".SH BUGS",
 "At http://bugs.darcs.net/ you can find a list of known",
 "bugs in Darcs.  Unknown bugs can be reported at that",
 "site (after creating an account) or by emailing the",
 "report to bugs@darcs.net.",
 -- ".SH EXAMPLE",
 -- FIXME:
 -- new project: init, rec -la;
 -- track upstream project: clone, pull -a;
 -- contribute to project: add, rec, push/send.
 ".SH SEE ALSO",
 "The Darcs website provides a lot of additional information.",
 "It can be found at http://darcs.net/",
 ".SH LICENSE",
 "Darcs is free software; you can redistribute it and/or modify",
 "it under the terms of the GNU General Public License as published by",
 "the Free Software Foundation; either version 2, or (at your option)",
 "any later version." ]
    where
      -- | A synopsis line for each command.  Uses 'foldl' because it is
      -- necessary to avoid blank lines from Hidden_commands, as groff
      -- translates them into annoying vertical padding (unlike TeX).
      synopsis :: Doc
      synopsis = foldl iter mempty commandControlList
          where iter :: Doc -> CommandControl -> Doc
                iter acc (GroupName _) = acc
                iter acc (HiddenCommand _) = acc
                iter acc (CommandData (c@SuperCommand {})) =
                    acc $$ vcat (map
                            (render (commandName c ++ " "))
                            (extractCommands (commandSubCommands c)))
                iter acc (CommandData c) = acc $$ render "" c
                render :: String -> DarcsCommand -> Doc
                render prefix c =
                    ".B darcs " <> text prefix <> text (commandName c) $$
                    vcat (map (text.mangle_args) (commandExtraArgHelp c)) $$
                    -- In the output, we want each command to be on its own
                    -- line, but we don't want blank lines between them.
                    ".br"

      -- | As 'synopsis', but make each group a subsection (.SS), and
      -- include the help text for each command.
      commands :: Doc
      commands = vsep $ map iter commandControlList
          where iter :: CommandControl -> Doc
                iter (GroupName x) = ".SS" <+> quoted x
                iter (HiddenCommand _) = mempty
                iter (CommandData (c@SuperCommand {})) =
                  vcat
                  [ ".B darcs " <> text (commandName c)
                  , text (mangle_args "subcommand")
                  , ".RS 4"
                  , commandHelp c
                  , ".RE"
                  ]
                  $+$
                  vsep (map (render (commandName c ++ " "))
                    (extractCommands (commandSubCommands c)))
                iter (CommandData c) = render "" c
                render :: String -> DarcsCommand -> Doc
                render prefix c =
                  vcat
                  [ ".B darcs " <> text prefix <> text (commandName c)
                  , vcat (map (text.mangle_args) (commandExtraArgHelp c))
                  , ".RS 4"
                  , commandHelp c
                  , ".RE"
                  ]

      -- | Now I'm showing off: mangle the extra arguments of Darcs commands
      -- so as to use the ideal format for manpages, italic words and roman
      -- punctuation.
      mangle_args :: String -> String
      mangle_args s =
          ".RI " ++ unwords (map show (groupBy cmp $ map toLower $ gank s))
              where cmp x y = isAlphaNum x == isAlphaNum y
                    gank (' ':'o':'r':' ':xs) = '|' : gank xs
                    gank (x:xs) = x : gank xs
                    gank [] = []

      environment :: Doc
      environment = vcat $ concat
                    [(".SS" <+> quoted (andClauses ks)) : map text ds
                     | (ks, ds) <- environmentHelp]

      prefFiles :: Doc
      prefFiles = vcat $ map go prefsFilesHelp
        where go (f,h) = ".SS" <+> quoted("_darcs/prefs/" <> f) $$ text h

      description = vcat
        [ "Unlike conventional revision control systems, Darcs is based on tracking"
        , "changes, rather than versions: it can and does automatically re-order"
        , "independent changes when needed. This means that in Darcs the state of"
        , "a repository should be regarded as a"
        , ".I set of patches"
        , "rather than a"
        , ".I sequence of versions."
        , ""
        , "Another distinguishing feature of darcs is that most commands are"
        , "interactive by default. For instance, `darcs record' (the equivalent of"
        , "what is usually called `commit') presents you with"
        , "each unrecorded change and asks you whether it should be included in"
        , "the patch to be recorded. Similarly, `darcs push' and `darcs pull'"
        , "present you with each patch, allowing you to select which patches to"
        , "push or pull."
        ]

markdownLines :: [String]
markdownLines =
 [ "# Commands", ""
 , unlines commands
 , "# Patterns"
 , "", unlines helpOnMatchers
 , "# Configuration"
 , "", unlines prefFiles
 , "# Environment variables"
 , "", unlines environment ]
   where
      prefFiles = concatMap go prefsFilesHelp
        where go (f,h) = ["## `_darcs/prefs/" ++ f ++ "`", "", h]

      environment :: [String]
      environment = intercalate [""]
                     [ renderEnv ks ds | (ks, ds) <- environmentHelp ]
        where
          renderEnv k d = ("## " ++ (intercalate ", " k)) : "" : d
      commands :: [String]
      commands = foldl iter [] commandControlList
      iter :: [String] -> CommandControl -> [String]
      iter acc (GroupName x) = acc ++ ["## " ++ x, ""]
      iter acc (HiddenCommand _) = acc
      iter acc (CommandData (c@SuperCommand {})) =
          acc ++ concatMap
                  (render (commandName c ++ " "))
                  (extractCommands (commandSubCommands c))
      iter acc (CommandData c) = acc ++ render "" c
      render :: String -> DarcsCommand -> [String]
      render prefix c =
          [ "### " ++ prefix ++ commandName c
          , "", "darcs " ++ prefix ++ commandName c ++ " [OPTION]... " ++
          unwords (commandExtraArgHelp c)
          , "", commandDescription c
          , "", renderString (commandHelp c)
          , "Options:", optionsMarkdown $ commandBasicOptions c
          , if null opts2 then ""
             else unlines ["Advanced Options:", optionsMarkdown opts2]
          ]
       where opts2 = commandAdvancedOptions c

environmentHelpEditor :: ([String], [String])
environmentHelpEditor = (["DARCS_EDITOR", "VISUAL", "EDITOR"],[
 "To edit a patch description of email comment, Darcs will invoke an",
 "external editor.  Your preferred editor can be set as any of the",
 "environment variables $DARCS_EDITOR, $VISUAL or $EDITOR.",
 "If none of these are set, nano is used.  If nano crashes or is not",
 "found in your PATH, vi, emacs, emacs -nw and (on Windows) edit are",
 "each tried in turn."])

environmentHelpPager :: ([String], [String])
environmentHelpPager = (["DARCS_PAGER", "PAGER"],[
 "Darcs will invoke a pager if the output of some command is longer",
 "than 20 lines. Darcs will use the pager specified by $DARCS_PAGER",
 "or $PAGER.  If neither are set, `less` will be used."])

environmentHelpTimeout :: ([String], [String])
environmentHelpTimeout = (["DARCS_CONNECTION_TIMEOUT"],[
 "Set the maximum time in seconds that darcs allows and connection to",
 "take. If the variable is not specified the default are 30 seconds.",
 "This option only works with curl."])

-- | There are two environment variables that we do not document:
-- - DARCS_USE_ISPRINT: deprecated, use DARCS_DONT_ESCAPE_ISPRINT.
-- - DARCS_TESTING_PREFS_DIR: used by the test suite to tell darcs
--                            where to find its configuration files.
