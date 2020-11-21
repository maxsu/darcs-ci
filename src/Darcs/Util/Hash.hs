--  Copyright (C) 2009-2011 Petr Rockai BSD3
--  Copyright (C) 2001, 2004 Ian Lynagh <igloo@earth.li>

module Darcs.Util.Hash
    ( Hash(..)
    , encodeBase16, decodeBase16, sha256, sha256sum, rawHash
    , match
    -- SHA1 related (patch metadata hash)
    , sha1PS, SHA1(..), showAsHex, sha1Xor, sha1zero, sha1short
    , sha1Show, sha1Read
 ) where

-- we currently have to depend on the memory package in addition to cryptonite
-- just so that we can import this single function
import Data.ByteArray ( convert )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16

import qualified Crypto.Hash as H

import Data.Char( intToDigit, ord )

import Data.Binary ( Binary(..), decode, encode )
import Data.Bits ( xor, shiftL, (.|.) )
import Data.Word ( Word8, Word32 )

import Darcs.Prelude


data Hash = SHA256 !B.ByteString
          | NoHash
            deriving (Show, Eq, Ord, Read)

-- | Produce a base16 (ascii-hex) encoded string from a hash. This can be
-- turned back into a Hash (see "decodeBase16". This is a loss-less process.
encodeBase16 :: Hash -> B.ByteString
encodeBase16 (SHA256 bs) = B16.encode bs
encodeBase16 NoHash = B.empty

-- | Take a base16-encoded string and decode it as a "Hash". If the string is
-- malformed, yields NoHash.
decodeBase16 :: B.ByteString -> Hash
decodeBase16 bs
  | B.length bs == 64
  , Right dbs <- B16.decode bs = SHA256 dbs
  | otherwise = NoHash

-- | Compute a sha256 of a (lazy) ByteString.
sha256 :: BL.ByteString -> Hash
sha256 bits = SHA256 (convert (H.hashlazy bits :: H.Digest H.SHA256))

-- | Same as previous but general purpose.
sha256sum :: B.ByteString -> String
sha256sum = BC.unpack . B16.encode . convert . H.hashWith H.SHA256

rawHash :: Hash -> B.ByteString
rawHash NoHash = error "Cannot obtain raw hash from NoHash."
rawHash (SHA256 s) = s

match :: Hash -> Hash -> Bool
NoHash `match` _ = False
_ `match` NoHash = False
x `match` y = x == y

data SHA1 = SHA1 !Word32 !Word32 !Word32 !Word32 !Word32
  deriving (Eq,Ord)

instance Show SHA1 where
  show = BC.unpack . sha1Show

instance Binary SHA1 where
  put (SHA1 a b c d e) = put a >> put b >> put c >> put d >> put e
  get = do a <- get; b <- get; c <- get; d <- get; e <- get; return (SHA1 a b c d e)

sha1Xor :: SHA1 -> SHA1 -> SHA1
sha1Xor (SHA1 a1 b1 c1 d1 e1) (SHA1 a2 b2 c2 d2 e2) =
  SHA1 (a1 `xor` a2) (b1 `xor` b2) (c1 `xor` c2) (d1 `xor` d2) (e1 `xor` e2)

sha1zero :: SHA1
sha1zero = SHA1 0 0 0 0 0

sha1short :: SHA1 -> Word32
sha1short (SHA1 a _ _ _ _) = a

sha1PS:: B.ByteString -> SHA1
sha1PS = fromArray . convert . H.hashWith H.SHA1 where
  fromArray = decode . BL.fromStrict

showAsHex :: Word32 -> String
showAsHex n = showIt 8 n ""
   where
    showIt :: Int -> Word32 -> String -> String
    showIt 0 _ r = r
    showIt i x r = case quotRem x 16 of
                       (y, z) -> let c = intToDigit (fromIntegral z)
                                 in c `seq` showIt (i-1) y (c:r)

-- | Parse a 'SHA1' directly from its B16 encoding, given as a 'B.ByteString',
-- or return 'Nothing'. The implementation is quite low-level and optimized
-- because the current implementation of RepoPatchV3 has to read lots of 'SHA1'
-- hashes, and profiling showed that this is a bottleneck.
sha1Read :: B.ByteString -> Maybe SHA1
sha1Read bs
  | B.length bs == 40
  , B.all is_hex bs =
    Just $ SHA1 (readWord 0) (readWord 8) (readWord 16) (readWord 24) (readWord 32)
  | otherwise = Nothing
  where
    readWord i = B.foldl' readByte 0 (B.take 8 (B.drop i bs))
    readByte :: Word32 -> Word8 -> Word32
    readByte r b = r `shiftL` 4 .|. (fromHex b)
    fromHex :: Word8 -> Word32
    fromHex b | btw_0_9 b = fromIntegral (b - ord_0)
              | btw_a_f b = fromIntegral (b - ord_a) + 10
              | otherwise = error "impossible case"
    ord_0 :: Word8
    ord_0 = fromIntegral (ord '0')
    ord_9 :: Word8
    ord_9 = fromIntegral (ord '9')
    ord_a :: Word8
    ord_a = fromIntegral (ord 'a')
    ord_f :: Word8
    ord_f = fromIntegral (ord 'f')
    btw_0_9 b = b >= ord_0 && b <= ord_9
    btw_a_f b = b >= ord_a && b <= ord_f
    is_hex b = btw_0_9 b || btw_a_f b

{-# INLINE sha1Show #-}
sha1Show :: SHA1 -> B.ByteString
sha1Show = B16.encode . BL.toStrict . encode
