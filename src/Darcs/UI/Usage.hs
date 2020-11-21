{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Usage
    ( getCommandHelp
    , getSuperCommandHelp
    , getCommandMiniHelp
    , usage
    , subusage
    ) where

import Darcs.Prelude

import Data.Functor.Compose
import System.Console.GetOpt( OptDescr(..), ArgDescr(..) )
import Darcs.UI.Options.All ( stdCmdActions )
import Darcs.UI.Commands
    ( CommandControl(..)
    , DarcsCommand(..)
    , commandName
    , commandDescription
    , getSubcommands
    , commandAlloptions
    )
import Darcs.UI.Options ( DarcsOptDescr, odesc )
import Darcs.Util.Printer
    ( Doc, text, vsep, ($$), vcat, hsep
    , renderString
    )

formatOptions :: [DarcsOptDescr a] -> [String]
formatOptions optDescrs = table
   where (ss,ls,ds)     = (unzip3 . concatMap fmtOpt) optDescrs
         table          = zipWith3 paste
                            shortPadded
                            (zipWith (++) (map (unlines' . init) ls)
                                          (sameLen $ map last ls))
                            ds
         shortPadded    = sameLen ss
         prePad         = replicate (3 + length (head shortPadded)) ' '
         -- Similar to unlines (additional ',' and padding):
         unlines'       = concatMap (\x -> x ++ ",\n" ++ prePad)
         -- Unchanged:
         paste x y z    = "  " ++ x ++ " " ++ y ++ "  " ++ z
         sameLen xs     = flushLeft ((maximum . map length) xs) xs
         flushLeft n xs = [ take n (x ++ repeat ' ') | x <- xs ]

-- Mild variant of the standard definition: 'losFmt' is a list rather than a
-- comma separated string.
fmtOpt :: DarcsOptDescr a -> [(String,[String],String)]
fmtOpt (Compose (Option sos los ad descr)) =
   case lines descr of
     []     -> [(sosFmt,losFmt,"")]
     (d:ds) ->  (sosFmt,losFmt,d) : [ ("",[],d') | d' <- ds ]
   where endBy _  []     = ""
         endBy ch [x]    = x ++ [ch]
         endBy ch (x:xs) = x ++ ch:' ':endBy ch xs
         sosFmt = endBy ',' (map fmtShort sos)
         losFmt = map (fmtLong ad) los

--------------------------------------------------------------------------------
-- Verbatim copies: these definitions aren't exported by System.Console.GetOpt
--------------------------------------------------------------------------------

fmtShort :: Char -> String
fmtShort so = "-" ++ [so]

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"
--------------------------------------------------------------------------------

usage :: [CommandControl] -> Doc
usage cs = vsep 
    [ "Usage: darcs COMMAND ..."
    , "Commands:" $$ usageHelper cs
    , vcat
      [ "Use 'darcs help COMMAND' or 'darcs COMMAND --help' for help on a single command."
      , "Use 'darcs help patterns' for help on patch matching."
      , "Use 'darcs help environment' for help on environment variables."
      , "Use 'darcs help manpage' to display help in the manpage format."
      , "Use 'darcs help markdown' to display help in the markdown format."
      , "Use 'darcs --version' to see the darcs version number."
      , "Use 'darcs --exact-version' to see a detailed darcs version."
      ]
    , "Check bug reports at http://bugs.darcs.net/"
    ]

subusage :: DarcsCommand -> Doc
subusage super = vsep
    [ superUsage super $$ text (commandDescription super)
    , usageHelper (getSubcommands super)
    , "Options:"
    , vcat $ map text $ formatOptions $ odesc stdCmdActions
    , commandHelp super
    ]

superUsage :: DarcsCommand -> Doc
superUsage super = hsep $ map text
    [ "Usage:"
    , commandProgramName super
    , commandName super
    , "SUBCOMMAND [OPTION]..."
    ]

usageHelper :: [CommandControl] -> Doc
usageHelper xs = vsep (groups xs)
  where
    groups [] = []
    groups (HiddenCommand _:cs) = groups cs
    groups (GroupName n:cs) =
      mempty : case groups cs of
        [] -> [text n]
        (g:gs) -> (text n $$ g) : gs
    groups (CommandData c:cs) =
      case groups cs of
        [] -> [cmdHelp c]
        (g:gs) -> (cmdHelp c $$ g) : gs

    cmdHelp c = text $ "  " ++
      padSpaces maxwidth (commandName c) ++
      commandDescription c

    padSpaces n s = s ++ replicate (n - length s) ' '

    maxwidth = maximum $ 15 : (map cwidth xs)

    cwidth (CommandData c) = length (commandName c) + 2
    cwidth _               = 0

getCommandMiniHelp :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandMiniHelp msuper cmd = renderString $ vsep
    [ getCommandHelpCore msuper cmd
    , hsep $ map text
        [ "See"
        , commandProgramName cmd
        , "help"
        , maybe "" ((++ " ") . commandName) msuper ++ commandName cmd
        , "for details."
        ]
    ]

getCommandHelp :: Maybe DarcsCommand -> DarcsCommand -> Doc
getCommandHelp msuper cmd = vsep
    [ getCommandHelpCore msuper cmd
    , subcommandsHelp
    , withHeading "Options:" basicOptionsHelp
    , withHeading "Advanced options:" advancedOptionsHelp
    , commandHelp cmd
    ]
  where
    withHeading _ [] = mempty
    withHeading h ls = vcat (text h : map text ls)

    (basic, advanced) = commandAlloptions cmd
    -- call formatOptions with combined options so that
    -- both get the same formatting
    (basicOptionsHelp, advancedOptionsHelp) =
        splitAt (length basic) $ formatOptions (basic ++ advanced)

    subcommandsHelp =
      case msuper of
        Nothing -> usageHelper (getSubcommands cmd)
        -- we don't want to list subcommands if we're already specifying them
        Just _ -> mempty

getSuperCommandHelp :: DarcsCommand -> Doc
getSuperCommandHelp super =
  vsep [superUsage super, usageHelper (getSubcommands super), commandHelp super]

getCommandHelpCore :: Maybe DarcsCommand -> DarcsCommand -> Doc
getCommandHelpCore msuper cmd = vcat
    [ hsep $
        [ "Usage:"
        , text $ commandProgramName cmd
        , maybe mempty (text . commandName) msuper
        , text $ commandName cmd
        , "[OPTION]..."
        ]
        ++ args_help
    , text $ commandDescription cmd
    ]
  where
    args_help = case cmd of
                    (DarcsCommand {}) -> map text $ commandExtraArgHelp cmd
                    _ -> []
