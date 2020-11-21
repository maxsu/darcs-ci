module Darcs.Test.Repository.Inventory where

import Darcs.Prelude

import Darcs.Repository.Inventory
    ( Inventory(..)
    , HeadInventory
    , ValidHash(..)
    , InventoryHash
    , PatchHash
    , PristineHash
    , mkValidHash
    , parseInventory
    , showInventory
    , skipPristineHash
    , peekPristineHash
    , pokePristineHash
    , prop_inventoryParseShow
    , prop_peekPokePristineHash
    , prop_skipPokePristineHash
    )
import Darcs.Patch.Info ( rawPatchInfo )
import Darcs.Util.Printer ( renderPS )

import Darcs.Test.Patch.Info ()

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit ( Assertion, (@=?) )
import Test.QuickCheck

testSuite :: Test
testSuite = testGroup "Darcs.Repository.Inventory"
 [ testProperty "parse/show roundtrips" prop_inventoryParseShow
 , testProperty "peek gets back what we poked" prop_peekPokePristineHash
 , testProperty "skip/poke roundtrips" prop_skipPokePristineHash
 , testCase "example1" (testInventory rawHeadInv1 headInv1)
 , testCase "example2" (testInventory rawHeadInv2 headInv2)
 ]

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary Inventory where
  arbitrary = uncurry Inventory <$> arbitrary

instance Arbitrary InventoryHash where
  arbitrary = arbitraryHash
instance Arbitrary PatchHash where
  arbitrary = arbitraryHash
instance Arbitrary PristineHash where
  arbitrary = arbitraryHash

arbitraryHash :: ValidHash h => Gen h
arbitraryHash = mkValidHash <$> do
  n <- elements [64, 75] -- see D.R.Cache.okayHash
  vectorOf n $ elements $ '-' : (['0'..'9'] ++ ['a'..'f'])

testInventory :: B.ByteString -> HeadInventory -> Assertion
testInventory raw (hash,inv) = do
  hash @=? peekPristineHash raw
  let rest = skipPristineHash raw
  Right inv @=? parseInventory rest
  rest @=? renderPS (showInventory inv)
  raw @=? renderPS (pokePristineHash hash rest)

headInv1 :: HeadInventory
headInv1 =
  ( mkValidHash "57fb9c1abbed1c0b880e2fffebe32a2163762b87e67e9bf4dcd3168e5abcad83"
  , Inventory
      { inventoryParent = Nothing
      , inventoryPatches =
          [ ( rawPatchInfo
                "20180311141206"
                "Add d/f and e."
                "tester"
                [ "Ignore-this: b541ff7ea385297c8ad07fe58016efa8" ]
                False
            , mkValidHash
                "0000000154-703d7811c2e3f1e1aa81e4be5fab31a291cc18158ec8a75733b6faa5fb406286"
            )
          , ( rawPatchInfo
                "20180311141206"
                "Move d/f to e/f."
                "tester"
                [ "Ignore-this: b71452c8a91c573f7e7fa2e8eb34afd1" ]
                False
            , mkValidHash
                "0000000106-4b1bc6db02d2eea04efe888b64ce853a416c14ae1ae43550b0137f11a8a8dfee"
            )
          ]
      }
  )

rawHeadInv1 :: B.ByteString
rawHeadInv1 = BC.pack
  "pristine:57fb9c1abbed1c0b880e2fffebe32a2163762b87e67e9bf4dcd3168e5abcad83\n\
  \[Add d/f and e.\n\
  \tester**20180311141206\n\
  \ Ignore-this: b541ff7ea385297c8ad07fe58016efa8\n\
  \] \n\
  \hash: 0000000154-703d7811c2e3f1e1aa81e4be5fab31a291cc18158ec8a75733b6faa5fb406286\n\
  \[Move d/f to e/f.\n\
  \tester**20180311141206\n\
  \ Ignore-this: b71452c8a91c573f7e7fa2e8eb34afd1\n\
  \] \n\
  \hash: 0000000106-4b1bc6db02d2eea04efe888b64ce853a416c14ae1ae43550b0137f11a8a8dfee\n\
  \"

headInv2 :: HeadInventory
headInv2 =
  ( mkValidHash "f2f70f1326252fc53077d7cd71769f405618829ba40a8f00f112ac97213f5f4b"
  , Inventory
      { inventoryParent =
          Just
            (mkValidHash
               "0000220070-6ef010a955c38fc4301787092979994bafd366eb50152b66e089deff649d35da")
      , inventoryPatches =
          [ ( rawPatchInfo
                "20160429142058"
                "TAG 2.12.0"
                "Guillaume Hoffmann <guillaumh@gmail.com>"
                [ "Ignore-this: 5c8cbe0424942686a2168f9e6fd8e35d" ]
                False
            , mkValidHash
                "0000088075-e1cc4489099cfff1df5875a8146dc012110c156f3dad839f4632d62ee2331e43"
            )
          , ( rawPatchInfo
                "20160429143015"
                "bump version to 2.13.0"
                "Guillaume Hoffmann <guillaumh@gmail.com>"
                [ "Ignore-this: 7468e30e96f3bf833f4e374e9cc7e515" ]
                False
            , mkValidHash
                "0000000198-0f5455b7c229e132a2fc2173dcce2b567f806c1d3eb1c37fd9fe8d8e42ef4fc9"
            )
          ]
      }
  )

rawHeadInv2 :: B.ByteString
rawHeadInv2 = BC.pack
  "pristine:f2f70f1326252fc53077d7cd71769f405618829ba40a8f00f112ac97213f5f4b\n\
  \Starting with inventory:\n\
  \0000220070-6ef010a955c38fc4301787092979994bafd366eb50152b66e089deff649d35da\n\
  \[TAG 2.12.0\n\
  \Guillaume Hoffmann <guillaumh@gmail.com>**20160429142058\n\
  \ Ignore-this: 5c8cbe0424942686a2168f9e6fd8e35d\n\
  \] \n\
  \hash: 0000088075-e1cc4489099cfff1df5875a8146dc012110c156f3dad839f4632d62ee2331e43\n\
  \[bump version to 2.13.0\n\
  \Guillaume Hoffmann <guillaumh@gmail.com>**20160429143015\n\
  \ Ignore-this: 7468e30e96f3bf833f4e374e9cc7e515\n\
  \] \n\
  \hash: 0000000198-0f5455b7c229e132a2fc2173dcce2b567f806c1d3eb1c37fd9fe8d8e42ef4fc9\n\
  \"
