-- Copyright (C) 2002-2003 David Roundy
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

module Darcs.Patch.Read
    ( ReadPatch(..)
    , readPatch
    , readPatchPartial
    , bracketedFL
    , peekfor
    , readFileName
    ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Control.Monad ( mzero )
import qualified Data.ByteString as B ( ByteString, null )
import qualified Data.ByteString.Char8 as BC ( ByteString, pack, stripPrefix )

import Darcs.Patch.Bracketed ( Bracketed(..), unBracketedFL )
import Darcs.Patch.Format
    ( FileNameFormat(..)
    , ListFormat(..)
    , PatchListFormat(..)
    )
import Darcs.Util.Parser
    ( Parser
    , checkConsumes
    , choice
    , lexChar
    , lexString
    , lexWord
    , parse
    )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal )

import Darcs.Util.ByteString ( decodeLocale, dropSpace, unpackPSFromUTF8 )
import Darcs.Util.Path ( AnchoredPath, decodeWhite, floatPath )

-- | This class is used to decode patches from their binary representation.
class ReadPatch p where
    readPatch' :: Parser (Sealed (p wX))

readPatchPartial :: ReadPatch p => B.ByteString -> Either String (Sealed (p wX), B.ByteString)
readPatchPartial = parse readPatch'

readPatch :: ReadPatch p => B.ByteString -> Either String (Sealed (p wX))
readPatch ps =
  case parse readPatch' ps of
    Left e -> Left e
    Right (p, leftover)
      | B.null (dropSpace leftover) -> Right p
      | otherwise -> Left $ unlines ["leftover:",show leftover]

instance ReadPatch p => ReadPatch (Bracketed p) where
    readPatch' = mapSeal Braced <$> bracketedFL readPatch' '{' '}'
                   <|>
                 mapSeal Parens <$> bracketedFL readPatch' '(' ')'
                   <|>
                 mapSeal Singleton <$> readPatch'

instance (ReadPatch p, PatchListFormat p) => ReadPatch (FL p) where
    readPatch'
        | ListFormatV1 <- patchListFormat :: ListFormat p
            = mapSeal unBracketedFL <$> readPatch'
        -- in the V2 format case, we only need to support () on reading, not {}
        -- for simplicity we just go through the same code path.
        | ListFormatV2 <- patchListFormat :: ListFormat p
            = mapSeal unBracketedFL <$> readPatch'
        | otherwise
            = read_patches
     where read_patches :: Parser (Sealed (FL p wX))
           read_patches = do --tracePeek "starting FL read"
                             -- checkConsumes is needed to make sure that something is read,
                             -- to avoid stack overflow when parsing FL (FL p)
                             mp <- (Just <$> checkConsumes readPatch') <|> return Nothing
                             case mp of
                               Just (Sealed p) -> do --tracePeek "found one patch"
                                                     Sealed ps <- read_patches
                                                     return $ Sealed (p:>:ps)
                               Nothing -> return $ Sealed NilFL
--           tracePeek x = do y <- peekInput
--                            traceDoc (greenText x $$ greenText (show $ sal_to_string y)) return ()

instance (ReadPatch p, PatchListFormat p) => ReadPatch (RL p) where
    readPatch' = mapSeal reverseFL <$> readPatch'

{-# INLINE bracketedFL #-}
bracketedFL :: forall p wX .
               (forall wY . Parser (Sealed (p wY))) -> Char -> Char -> Parser (Sealed (FL p wX))
bracketedFL parser pre post =
    peekforc pre bfl mzero
        where bfl :: forall wZ . Parser (Sealed (FL p wZ))
              bfl = peekforc post (return $ Sealed NilFL)
                                  (do Sealed p <- parser
                                      Sealed ps <- bfl
                                      return $ Sealed (p:>:ps))

{-# INLINE peekforc #-}
peekforc :: Char -> Parser a -> Parser a -> Parser a
peekforc c ifstr ifnot = choice [ lexChar c >> ifstr
                                , ifnot ]

peekfor :: BC.ByteString -> Parser a -> Parser a -> Parser a
peekfor ps ifstr ifnot = choice [ do lexString ps
                                     ifstr
                                , ifnot ]
{-# INLINE peekfor #-}

-- See also Darcs.Patch.Show.formatFileName.
readFileName :: FileNameFormat -> Parser AnchoredPath
readFileName fmt = do
  raw <- lexWord
  case BC.stripPrefix (BC.pack "./") raw of
    Nothing -> fail $ "invalid file path"
    Just raw' -> return $ convert fmt raw'
  where
    convert FileNameFormatV1 =
      floatPath . decodeWhite . decodeLocale . BC.pack . unpackPSFromUTF8
    convert FileNameFormatV2 =
      floatPath . decodeWhite . decodeLocale
    convert FileNameFormatDisplay = error "readFileName called with FileNameFormatDisplay"
