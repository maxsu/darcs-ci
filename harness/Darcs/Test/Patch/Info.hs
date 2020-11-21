-- Copyright (C) 2009 Reinier Lamers
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

-- | This module contains tests for the code in Darcs.Patch.Info. Most of them
--   are about the UTF-8-encoding of patch metadata.
module Darcs.Test.Patch.Info ( testSuite ) where

import Prelude hiding ( pi )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString as B ( ByteString, pack )
import qualified Data.ByteString.Char8 as BC ( pack, unpack )
import Data.List ( sort , isPrefixOf, partition )
import Data.Maybe ( isNothing )
import Data.Text as T ( find, any )
import Data.Text.Encoding ( decodeUtf8With )
import Data.Text.Encoding.Error ( lenientDecode )
import Data.Word ( Word32 )
import Numeric ( showHex )
import Test.QuickCheck ( Arbitrary(arbitrary), oneof, listOf, choose, shrink
                       , Gen, suchThat, scale )
import Test.QuickCheck.Gen ( chooseAny )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework (Test, testGroup)
-- import Text.Show.Pretty ( ppShow )

import Darcs.Patch.Info
    ( PatchInfo(..), rawPatchInfo, showPatchInfo, readPatchInfo
    , piLog, piAuthor, piName, validDate, validLog, validAuthor
    , validDatePS, validLogPS, validAuthorPS, piDateString
    )
import Darcs.Test.TestOnly.Instance ()
import Darcs.Util.Parser ( parse )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Util.ByteString
    ( decodeLocale, packStringToUTF8, unpackPSFromUTF8, linesPS )
import Darcs.Util.Printer ( renderPS )

testSuite :: Test
testSuite = testGroup "Darcs.Patch.Info"
  [ metadataDecodingTest
  , metadataEncodingTest
  , packUnpackTest
  , parseUnparseTest
  ]

-- | A newtype wrapping String so we can make our own random generator for it.
newtype UnicodeString = UnicodeString { asString :: String }
        deriving (Show, Eq, Ord)

-- | A newtype wrapping PatchInfo that has a random generator that generates
--   both UTF-8-encoded and non-encoded PatchInfo's.
newtype UTF8OrNotPatchInfo = UTF8OrNotPatchInfo PatchInfo deriving (Eq, Ord)

-- | A newtype wrapping PatchInfo, which has a random generator that generates
--   only UTF-8-encoded PatchInfo's.
newtype UTF8PatchInfo = UTF8PatchInfo PatchInfo deriving (Eq, Ord)

-- Note that this instance only creates valid unicode strings. It does not
-- generate lone surrogates, for instance, as these would fail the
-- packUnpackTest below.
instance Arbitrary UnicodeString where
    -- 0x10ffff is the highest Unicode code point ; 0xd800 - 0xdfff are
    -- surrogates. '\xfffd' is excluded because it is used as a marker
    -- for UTF-8 test failure.
    arbitrary = UnicodeString `fmap` listOf (oneof [choose ('\0', '\xd799')
                                                   ,choose ('\xe000', '\xfffc')
                                                   ,choose ('\xfffe', '\x10ffff')])

instance Show UTF8PatchInfo where
    show = withUTF8PatchInfo rawPatchInfoShow
instance Show UTF8OrNotPatchInfo where
    show = withUTF8OrNotPatchInfo rawPatchInfoShow

-- | Shows a PatchInfo, outputting every byte and clearly marking what is what
rawPatchInfoShow :: PatchInfo -> String
rawPatchInfoShow = {- ppShow -} show

instance Arbitrary UTF8PatchInfo where
    arbitrary = UTF8PatchInfo `fmap` arbitraryUTF8PatchInfo
    shrink (UTF8PatchInfo pi) = map UTF8PatchInfo (shrinkPatchInfo pi)

instance Arbitrary UTF8OrNotPatchInfo where
    arbitrary = UTF8OrNotPatchInfo `fmap` oneof ([arbitraryUTF8PatchInfo,
                                                  arbitraryUnencodedPatchInfo])
    shrink (UTF8OrNotPatchInfo pi) = map UTF8OrNotPatchInfo (shrinkPatchInfo pi)

-- Generate a random "Ignore-this:" line that makes sure that separately
-- generated PatchInfos are not equal
generateJunk :: Gen String
generateJunk =
  fmap (("Ignore-this: " ++) . concatMap (flip showHex "")) $
  sequence $ replicate 5 (chooseAny :: Gen Word32) 

-- | Generate arbitrary patch metadata.
-- Note : We must NOT use 'patchinfo' from Darcs.Patch.Info
-- with unsafePerformIO here because this breaks  the parse/unparse test
-- (the added junk will be different on each call).
arbitraryUTF8PatchInfo :: Gen PatchInfo
arbitraryUTF8PatchInfo = do
    d <- arbitrary `suchThat` validDate
    n <- (asString `fmap` arbitrary) `suchThat` validLog
    a <- (asString `fmap` arbitrary) `suchThat` validAuthor
    l <- lines `fmap` scale (* 2) arbitrary
    junk <- generateJunk
    i <- arbitrary
    return $ rawPatchInfo d n a (l ++ [junk]) i

-- | Generate arbitrary patch metadata that has totally arbitrary byte strings
--   as its name, date, author and log, as well as an arbitrary "legacy
--   inverted" setting.
arbitraryUnencodedPatchInfo :: Gen PatchInfo
arbitraryUnencodedPatchInfo = do
    d <- arbitraryByteString `suchThat` validDatePS
    n <- arbitraryByteString `suchThat` validLogPS
    a <- arbitraryByteString `suchThat` validAuthorPS
    l <- linesPS `fmap` scale (* 2) arbitraryByteString
    junk <- generateJunk
    i <- arbitrary
    return (PatchInfo d n a (l ++ [BC.pack junk]) i)

arbitraryByteString :: Gen B.ByteString
arbitraryByteString = B.pack <$> listOf arbitrary

-- | Test that anything produced by the 'patchinfo' function is valid UTF-8
metadataEncodingTest :: Test
metadataEncodingTest =
    testProperty "Testing patch metadata encoding" propMetadataEncoding

propMetadataEncoding :: UTF8PatchInfo -> Bool
propMetadataEncoding (UTF8PatchInfo patchInfo) =
    encodingOK (_piAuthor patchInfo)
    && encodingOK (_piName patchInfo)
    && all encodingOK (_piLog patchInfo)
  where
    encodingOK = isNothing . T.find (=='\xfffd') . decodeUtf8With lenientDecode

-- | Test that metadata in patches are decoded as UTF-8 or locale depending on
-- whether they're valid UTF-8.
metadataDecodingTest :: Test
metadataDecodingTest = testProperty "Testing patch metadata decoding" propMetadataDecoding

propMetadataDecoding :: UTF8OrNotPatchInfo -> Bool
propMetadataDecoding (UTF8OrNotPatchInfo patchInfo) =
    utf8OrLocale (_piAuthor patchInfo) == piAuthor patchInfo
    && utf8OrLocale (_piName patchInfo) == piName patchInfo
    && map utf8OrLocale (_piLog patchInfo) `superset` piLog patchInfo
  where
    utf8OrLocale bs =
      if isValidUTF8 bs then unpackPSFromUTF8 bs else decodeLocale bs

isValidUTF8 :: B.ByteString -> Bool
isValidUTF8 = not . T.any (=='\xfffd') . decodeUtf8With lenientDecode

packUnpackTest :: Test
packUnpackTest = testProperty "Testing UTF-8 packing and unpacking" $
    \uString -> asString uString == (unpackPSFromUTF8 . packStringToUTF8) (asString uString)

superset :: Ord a => [a] -> [a] -> Bool
superset a b = sorted_superset (sort a) (sort b)
  where sorted_superset (x:xs) (y:ys) | x == y = sorted_superset xs ys
                                      | x <  y = sorted_superset xs (y:ys)
                                      | otherwise = False
        sorted_superset []     (_:_)           = False
        sorted_superset _      []              = True

withUTF8PatchInfo :: (PatchInfo -> a) -> UTF8PatchInfo -> a
withUTF8PatchInfo f mpi = case mpi of
                            UTF8PatchInfo pinf -> f pinf
withUTF8OrNotPatchInfo :: (PatchInfo -> a) -> UTF8OrNotPatchInfo -> a
withUTF8OrNotPatchInfo f mpi = case mpi of
                                 UTF8OrNotPatchInfo pinf -> f pinf

parseUnparseTest :: Test
parseUnparseTest = testProperty "parse . show == id" propParseUnparse

parsePatchInfo :: B.ByteString -> Either String PatchInfo
parsePatchInfo = fmap fst . parse readPatchInfo

unparsePatchInfo :: PatchInfo -> B.ByteString
unparsePatchInfo = renderPS . showPatchInfo ForStorage

-- Once generated, we assume that shrinking will preserve UTF8ness etc,
-- so we reuse this function for all the various Arbitrary instances
shrinkPatchInfo :: PatchInfo -> [PatchInfo]
shrinkPatchInfo pi =
  go shrink return return return <|>
  go return shrink return return <|>
  go return return shrink return <|>
  go return return return shrink
  where
    go f1 f2 f3 f4 = do
      sn <- f1 (piName pi)
      sa <- f2 (piAuthor pi)
      sl <- f3 logLines
      i <- f4 (_piLegacyIsInverted pi)
      return $ rawPatchInfo (piDateString pi) sn sa (sl ++ junkLines) i
    -- We need to be careful to preserve the junk lines to prevent creating
    -- two identical PatchInfos from different ones, which would break darcs' invariants
    -- and cause a genuine failure to be shrunk into a spurious one.
    (junkLines, logLines) = partition (isPrefixOf "Ignore-this:") . map BC.unpack . _piLog $ pi

instance Arbitrary PatchInfo where
    arbitrary = arbitraryUnencodedPatchInfo
    shrink = shrinkPatchInfo

propParseUnparse :: PatchInfo -> Bool
propParseUnparse pi = Right pi == parsePatchInfo (unparsePatchInfo pi)
