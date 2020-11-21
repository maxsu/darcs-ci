-- Copyright (C) 2008 Eric Kow
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

-- |
-- Copyright   : 2008 Eric Kow
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable
--
-- This modules provides rudimentary natural language generation
-- (NLG) utilities.  That is, generating natural language from a
-- machine representation.  Initially, only English is supported at
-- all.  Representations are implemented for:
--
--  * countable nouns (plurality); and
--  * lists of clauses (foo, bar and/or baz).

module Darcs.Util.English where

import Darcs.Prelude

import Data.Char (toUpper)
import Data.List (isSuffixOf)

import Darcs.Util.Printer ( Doc, vcat, text )

-- | > englishNum 0 (Noun "watch") "" == "watches"
--   > englishNum 1 (Noun "watch") "" == "watch"
--   > englishNum 2 (Noun "watch") "" == "watches"
englishNum :: Countable n => Int -> n -> ShowS
englishNum x = if x == 1 then singular else plural

-- | Things that have a plural and singular spelling
class Countable a where
    plural :: a -> ShowS
    singular :: a -> ShowS

-- | This only distinguishes between nouns with a final -ch,
--   and nouns which do not.
--   More irregular nouns will just need to have their own type
--
--   > plural (Noun "batch") "" == "batches"
--   > plural (Noun "bat")   "" == "bats"
--   > plural (Noun "mouse") "" == "mouses" -- :-(
newtype Noun = Noun String
data Pronoun = It

instance Countable Noun where
    -- more irregular nouns will just need to have their own type
    plural (Noun s) | "ch" `isSuffixOf` s = showString s .  showString "es"
    plural (Noun s) | "y" `isSuffixOf` s
                      && length s > 1
                      && last (init s) `notElem` "aeiou" =
                            showString (init s) . showString "ies"
    plural (Noun s) = showString s . showChar 's'
    singular (Noun s) =  showString s

instance Countable Pronoun where
    plural It = showString "them"
    singular It = showString "it"

-- | > singular This (Noun "batch") "" == "this batch"
--   > plural   This (Noun "batch") "" == "these batches"
data This = This Noun

instance Countable This where
    plural (This s)   = showString "these "  . plural s
    singular (This s) = showString "this "   . singular s

-- | Given a list of things, combine them thusly:
--
--   > orClauses ["foo", "bar", "baz"] == "foo, bar or baz"
andClauses, orClauses :: [String] -> String
andClauses = itemize "and"
orClauses = itemize "or"

anyOfClause :: [String] -> Doc
anyOfClause names = if length names > 1 then text "any of" else mempty

itemizeVertical :: Int -> [String] -> Doc
itemizeVertical indent = vcat . map (text . ((replicate indent ' ' ++ "- ") ++))

-- Should not be called with an empty list since this usually
-- prints an extra space. We allow it for compatibility.
itemize :: String -> [String] -> String
itemize _ [] = "" -- error "precondition in Darcs.Util.English.itemize"
itemize _ [x] = x
itemize sep [x,x'] = unwords [x, sep, x']
itemize sep (x:x':xs) = itemize' x x' xs where
  itemize' y y' [] = unwords [y ++ ",", sep, y']
  itemize' y y' (y'':ys) = unwords [y ++ ",", itemize' y' y'' ys]

presentParticiple :: String -> String
presentParticiple v | last v == 'e' = init v ++ "ing"
                     | otherwise = v ++ "ing"

-- | Capitalize the first letter of a word
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
