--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.UI.Commands.Replace
    ( replace
    , defaultToks
    ) where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Char ( isSpace )
import Data.List.Ordered ( nubSort )
import Data.Maybe ( fromJust, isJust )
import Control.Monad ( unless, filterM, void, when )
import Darcs.Util.Tree( readBlob, modifyTree, findFile, TreeItem(..), Tree
                      , makeBlobBS )
import Darcs.Util.Path( AbsolutePath )
import Darcs.UI.Flags
    ( DarcsFlag
    , verbosity, useCache, dryRun, umask, diffAlgorithm, pathsFromArgs )
import Darcs.UI.Options ( (^), odesc, ocheck, defaultFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Repository.Flags ( UpdatePending(..) )
import Darcs.UI.Commands ( DarcsCommand(..), withStdOpts, nodefaults, amInHashedRepository )
import Darcs.UI.Completion ( knownFileArgs )
import Darcs.Repository.Diff( treeDiff )
import Darcs.Patch ( PrimPatch, tokreplace, forceTokReplace
                   , maybeApplyToTree )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.RegChars ( regChars )
import Darcs.Repository
    ( withRepoLock
    , RepoJob(..)
    , addToPending
    , applyToWorking
    , readUnrecorded
    )
import Darcs.Patch.TokenReplace ( defaultToks )
import Darcs.Repository.Prefs ( FileType(TextFile) )
import Darcs.Util.Path ( AnchoredPath, displayPath )
import Darcs.Util.Printer ( Doc, text )
import Darcs.Util.SignalHandler ( withSignalsBlocked )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+), concatFL, toFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal, FreeLeft, Gap(..), unFreeLeft, unseal )

replaceDescription :: String
replaceDescription = "Substitute one word for another."

replaceHelp :: Doc
replaceHelp = text $
 "In addition to line-based patches, Darcs supports a limited form of\n" ++
 "lexical substitution.  Files are treated as sequences of words, and\n" ++
 "each occurrence of the old word is replaced by the new word.\n" ++
 "This is intended to provide a clean way to rename a function or\n" ++
 "variable.  Such renamings typically affect lines all through the\n" ++
 "source code, so a traditional line-based patch would be very likely to\n" ++
 "conflict with other branches, requiring manual merging.\n" ++
 "\n" ++
 "Files are tokenized according to one simple rule: words are strings of\n" ++
 "valid token characters, and everything between them (punctuation and\n" ++
 -- FIXME: this heuristic is ham-fisted and silly.  Can we drop it?
 "whitespace) is discarded.  By default, valid token characters are\n" ++
 "letters, numbers and the underscore (i.e. `[A-Za-z0-9_]`).  However if\n" ++
 "the old and/or new token contains either a hyphen or period, BOTH\n" ++
 "hyphen and period are treated as valid (i.e. `[A-Za-z0-9_.-]`).\n" ++
 "\n" ++
 "The set of valid characters can be customized using the `--token-chars`\n" ++
 "option.  The argument must be surrounded by square brackets.  If a\n" ++
 "hyphen occurs between two characters in the set, it is treated as a\n" ++
 "set range.  For example, in most locales `[A-Z]` denotes all uppercase\n" ++
 "letters.  If the first character is a caret, valid tokens are taken to\n" ++
 "be the complement of the remaining characters.  For example, `[^:\\n]`\n" ++
 "could be used to match fields in the passwd(5), where records and\n" ++
 "fields are separated by newlines and colons respectively.\n" ++
 "\n" ++
 "If you choose to use `--token-chars`, you are STRONGLY encouraged to do\n" ++
 "so consistently.  The consequences of using multiple replace patches\n" ++
 "with different `--token-chars` arguments on the same file are not well\n" ++
 "tested nor well understood.\n" ++
 "\n" ++
 "By default Darcs will refuse to perform a replacement if the new token\n" ++
 "is already in use, because the replacements would be not be\n" ++
 "distinguishable from the existing tokens.  This behaviour can be\n" ++
 "overridden by supplying the `--force` option, but an attempt to `darcs\n" ++
 "rollback` the resulting patch will affect these existing tokens.\n" ++
 "\n" ++
 "Limitations:\n" ++
 "\n" ++
 "The tokenizer treats files as byte strings, so it is not possible for\n" ++
 "`--token-chars` to include multi-byte characters, such as the non-ASCII\n" ++
 "parts of UTF-8.  Similarly, trying to replace a \"high-bit\" character\n" ++
 "from a unibyte encoding will also result in replacement of the same\n" ++
 "byte in files with different encodings.  For example, an acute a from\n" ++
 "ISO 8859-1 will also match an alpha from ISO 8859-7.\n" ++
 "\n" ++
 "Due to limitations in the patch file format, `--token-chars` arguments\n" ++
 "cannot contain literal whitespace.  For example, `[^ \\n\\t]` cannot be\n" ++
 "used to declare all characters except the space, tab and newline as\n" ++
 "valid within a word, because it contains a literal space.\n" ++
 "\n" ++
 "Unlike POSIX regex(7) bracket expressions, character classes (such as\n" ++
 "`[[:alnum:]]`) are NOT supported by `--token-chars`, and will be silently\n" ++
 "treated as a simple set of characters.\n"

replace :: DarcsCommand
replace = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "replace"
    , commandHelp = replaceHelp
    , commandDescription = replaceDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = [ "<OLD>"
                            , "<NEW>"
                            , "<FILE> ..."
                            ]
    , commandCommand = replaceCmd
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = replaceArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc replaceAdvancedOpts
    , commandBasicOptions = odesc replaceBasicOpts
    , commandDefaults = defaultFlags replaceOpts
    , commandCheckOptions = ocheck replaceOpts
    }
  where
    replaceBasicOpts = O.tokens ^ O.forceReplace ^ O.repoDir
    replaceAdvancedOpts = O.useIndex ^ O.umask
    replaceOpts = replaceBasicOpts `withStdOpts` replaceAdvancedOpts

replaceArgs  :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO [String]
replaceArgs fps flags args =
  if length args < 2
    then return []
    else knownFileArgs fps flags args

replaceCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
replaceCmd fps opts (old : new : args@(_ : _)) =
  withRepoLock  (dryRun ? opts) (useCache ? opts) YesUpdatePending (umask ? opts) $ RepoJob $
    \repository -> do
        paths <- nubSort <$> pathsFromArgs fps args
        when (null paths) $ fail "No valid repository paths were given."
        toks <- chooseToks (O.tokens ? opts) old new
        let checkToken tok = unless (isTok toks tok) $
                                 fail $ "'" ++ tok ++ "' is not a valid token!"
        mapM_ checkToken [ old, new ]
        working <- readUnrecorded repository (O.useIndex ? opts) Nothing
        files <- filterM (exists working) paths
        Sealed replacePs <- mapSeal concatFL . toFL <$>
            mapM (doReplace toks working) files
        withSignalsBlocked $ do
          -- Note: addToPending takes care of commuting the replace patch and
          -- everything it depends on past the diff between pending and working
          addToPending repository (O.useIndex ? opts) replacePs
          void $ applyToWorking repository (verbosity ? opts) replacePs
  where
    exists tree file = if isJust $ findFile tree file
                           then return True
                           else do putStrLn $ skipmsg file
                                   return False

    skipmsg f = "Skipping file '" ++ displayPath f
                ++ "' which isn't in the repository."

    doReplace :: forall prim . (PrimPatch prim,
              ApplyState prim ~ Tree) => String -> Tree IO
              -> AnchoredPath -> IO (FreeLeft (FL prim))
    doReplace toks work f = do
        workReplaced <- maybeApplyToTree replacePatch work
        case workReplaced of
          Just _ -> do
            return $ joinGap (:>:) (freeGap replacePatch) gapNilFL
          Nothing
            | O.forceReplace ? opts -> getForceReplace f toks work
            | otherwise -> putStrLn existsMsg >> return gapNilFL
      where
        -- FIXME Why do we say "perhaps"? Aren't we sure? Are there other
        -- reasons maybeApplyToTree can fail and what to do about them?
        existsMsg = "Skipping file '" ++ displayPath f ++ "'\nPerhaps the working"
                    ++ " version of this file already contains '" ++ new
                    ++ "'?\nUse the --force option to override."
        gapNilFL = emptyGap NilFL
        replacePatch = tokreplace f toks old new

    ftf _ = TextFile

    -- | getForceReplace returns the list of patches that consists first of
    -- hunk patches to normalise all occurences of the target token (changing
    -- them back to the source token) and then the replace patches from
    -- oldToken -> newToken.
    getForceReplace :: PrimPatch prim
                    => AnchoredPath -> String -> Tree IO -> IO (FreeLeft (FL prim))
    getForceReplace path toks tree = do
        content <- readBlob $ fromJust $ findFile tree path
        let newcontent = forceTokReplace toks (BC.pack new) (BC.pack old)
                            (B.concat $ BL.toChunks content)
            tree' = modifyTree tree path . Just . File $ makeBlobBS newcontent
        normaliseNewTokPatch <- treeDiff (diffAlgorithm ? opts) ftf tree tree'
        unless (unseal nullFL (unFreeLeft normaliseNewTokPatch)) $
            putStrLn $ "Don't be surprised!\n"
                       ++ "I've changed all instances of '" ++ new ++ "' to '"
                       ++ old ++ "' first\n"
                       ++ "so that darcs replace can token-replace them"
                       ++ " back into '" ++ new ++ "' again."
        return . joinGap (+>+) normaliseNewTokPatch $ freeGap $
            tokreplace path toks old new :>: NilFL
replaceCmd _ _ [_, _] = fail "You need to supply a list of files to replace in!"
replaceCmd _ _ _ = fail "Usage: darcs replace <OLD> <NEW> <FILE>..."

filenameToks :: String
filenameToks = "A-Za-z_0-9\\-\\."

-- | Given a set of characters and a string, returns true iff the string
-- contains only characters from the set. A set beginning with a caret (@^@) is
-- treated as a complementary set.
isTok :: String -> String -> Bool
isTok _ "" = False
isTok toks s = all (regChars toks) s

-- | This function checks for @--token-chars@ on the command-line. If found,
-- it validates the argument and returns it, without the surrounding square
-- brackets. Otherwise, it returns either 'defaultToks' or 'filenameToks' as
-- explained in 'replaceHelp'.
--
-- Note: Limitations in the current replace patch file format prevents tokens
-- and token-char specifiers from containing any whitespace.
chooseToks :: Maybe String -> String -> String -> IO String
chooseToks (Just t) a b
    | length t <= 2 =
        badTokenSpec $ "It must contain more than 2 characters, because it"
                       ++ " should be enclosed in square brackets"
    | head t /= '[' || last t /= ']' =
        badTokenSpec "It should be enclosed in square brackets"
    | '^' == head tok && length tok == 1 =
        badTokenSpec "Must be at least one character in the complementary set"
    | any isSpace t =
        badTokenSpec "Space is not allowed in the spec"
    | any isSpace a = badTokenSpec $ spaceyToken a
    | any isSpace b = badTokenSpec $ spaceyToken b
    | not (isTok tok a) = badTokenSpec $ notAToken a
    | not (isTok tok b) = badTokenSpec $ notAToken b
    | otherwise = return tok
  where
    tok = init $ tail t :: String
    badTokenSpec msg = fail $ "Bad token spec: '" ++ t ++ "' (" ++ msg ++ ")"
    spaceyToken x = x ++ " must not contain any space"
    notAToken x = x ++ " is not a token, according to your spec"

chooseToks Nothing a b =
    if isTok defaultToks a && isTok defaultToks b
      then return defaultToks
      else return filenameToks
