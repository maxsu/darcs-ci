--  Copyright (C) 2002-2005,2007 David Roundy
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

module Darcs.Test.Misc ( testSuite ) where

import Darcs.Prelude

import Darcs.Util.ByteString
    ( unpackPSFromUTF8, fromHex2PS, fromPS2Hex
    , propHexConversion
    , prop_unlinesPS_linesPS_left_inverse
    , prop_linesPS_length
    , prop_unlinesPS_length
    , spec_betweenLinesPS
    , betweenLinesPS
    )
import Darcs.Util.Diff.Myers ( shiftBoundaries )

import Darcs.Test.Misc.CommandLine ( commandLineTestSuite )
import qualified Darcs.Test.Misc.Encoding as Encoding
import qualified Darcs.Test.Misc.Graph as Graph
import qualified Darcs.Test.Misc.URL as URL

import qualified Data.ByteString.Char8 as BC ( unpack, pack, snoc )
import qualified Data.ByteString as B ( ByteString, pack, empty, null )
import Data.Char ( ord )
import Data.Array.Base
import Control.Monad.ST
import Test.HUnit ( assertBool, assertEqual, assertFailure )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test, testGroup )
import Test.QuickCheck


testSuite :: Test
testSuite = testGroup ""
 [ byteStringUtilsTestSuite
 , lcsTestSuite
 , commandLineTestSuite
 , Encoding.testSuite
 , Graph.testSuite
 , URL.testSuite
 ]


-- ----------------------------------------------------------------------
-- * Darcs.Util.ByteString
-- ----------------------------------------------------------------------

byteStringUtilsTestSuite :: Test
byteStringUtilsTestSuite = testGroup "Darcs.Util.ByteString"
  [ testCase "UTF-8 packing and unpacking preserves 'hello world'"
           (assertBool "" (unpackPSFromUTF8 (BC.pack "hello world") == "hello world"))
  , testCase "Checking that hex packing and unpacking preserves 'hello world'"
           (assertEqual "" (BC.unpack (fromHex2PS $ fromPS2Hex $ BC.pack "hello world"))
                           "hello world")
  , testProperty "Checking that hex conversion works" propHexConversion
  , testProperty "unlinesPS is left inverse of linesPS" prop_unlinesPS_linesPS_left_inverse
  , testProperty "linesPS length property" prop_linesPS_length
  , testProperty "unlinesPS length property" prop_unlinesPS_length
  , testProperty "betweenLinesPS behaves like its spec" prop_betweenLinesPS
  ]

-- tweak the probabilities in favor of newline characters
instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack $ listOf $ frequency
    [ (1, return (fromIntegral (ord '\n')))
    , (4, arbitrary)
    ]

-- betweenLinesPS and spec_betweenLinesPS are equivalent only
-- if certain conditions are met
prop_betweenLinesPS :: B.ByteString -> B.ByteString -> B.ByteString -> Property
prop_betweenLinesPS start end ps =
  not (B.null start) && not (B.null end)
  ==> betweenLinesPS start end (twist ps) == spec_betweenLinesPS start end (twist ps)
  where
    twist s
      | B.null s = s
      | otherwise = s `BC.snoc` '\n'

-- ----------------------------------------------------------------------
-- * LCS
-- Here are a few quick tests of the shiftBoundaries function.
-- ----------------------------------------------------------------------

lcsTestSuite :: Test
lcsTestSuite = testGroup "LCS"
 [ testCase "lcs code" (mapM_ assertFailure showLcsTests)
 ]

showLcsTests :: [String]
showLcsTests = concatMap checkKnownShifts knownShifts
checkKnownShifts :: ([Int],[Int],String,String,[Int],[Int])
                   -> [String]
checkKnownShifts (ca, cb, sa, sb, ca', cb') = runST (
    do ca_arr <- newListArray (0, length ca) $ toBool (0:ca)
       cb_arr <- newListArray (0, length cb) $ toBool (0:cb)
       let p_a = listArray (0, length sa) $ B.empty:(toPS sa)
           p_b = listArray (0, length sb) $ B.empty:(toPS sb)
       shiftBoundaries ca_arr cb_arr p_a 1 1
       shiftBoundaries cb_arr ca_arr p_b 1 1
       ca_res <- fmap (fromBool . tail) $ getElems ca_arr
       cb_res <- fmap (fromBool . tail) $ getElems cb_arr
       return $ if ca_res  == ca' && cb_res == cb' then []
                else ["shiftBoundaries failed on "++sa++" and "++sb++" with "
                      ++(show (ca,cb))++" expected "++(show (ca', cb'))
                      ++" got "++(show (ca_res, cb_res))++"\n"])
 where toPS = map (\c -> if c == ' ' then B.empty else BC.pack [c])
       toBool = map (>0)
       fromBool = map (\b -> if b then 1 else 0)

knownShifts :: [([Int],[Int],String,String,[Int],[Int])]
knownShifts =
  [([0,0,0],[0,1,0,1,0],"aaa","aaaaa",
    [0,0,0],[0,0,0,1,1]),
   ([0,1,0],[0,1,1,0],"cd ","c a ",
    [0,1,0],[0,1,1,0]),
   ([1,0,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,1,1,1,1,0,0,0], "fg{} if{}","dg{} ih{} if{}",
    [1,0,0,0,0,0,0,0,0],[1,0,0,0,0,1,1,1,1,1,0,0,0,0]), -- prefer empty line at end
   ([0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,1,1,1,1,0,0,0], "fg{} if{}","fg{} ih{} if{}",
    [0,0,0,0,0,0,0,0,0],[0,0,0,0,0,1,1,1,1,1,0,0,0,0]), -- prefer empty line at end
   ([],[1,1],"","aa",[],[1,1]),
   ([1,1],[],"aa","",[1,1],[])]
