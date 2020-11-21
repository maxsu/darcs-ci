{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Darcs.Util.ByteString
-- Copyright   :  (c) The University of Glasgow 2001,
--                    David Roundy 2003-2005
-- License : GPL (I'm happy to also license this file BSD style but don't
--           want to bother distributing two license files with darcs.
--
-- Maintainer  :  droundy@abridgegame.org
-- Stability   :  experimental
-- Portability :  portable
--
-- GZIp and MMap IO for ByteStrings, encoding utilities, and miscellaneous
-- functions for Data.ByteString
--
module Darcs.Util.ByteString
    (
    -- * IO with mmap or gzip
      gzReadFilePS
    , mmapFilePS
    , gzWriteFilePS
    , gzWriteFilePSs
    , gzReadStdin
    , gzWriteHandle
    , FileSegment
    , readSegment
    -- * gzip handling
    , isGZFile
    , gzDecompress
    -- * list utilities
    , dropSpace
    , linesPS
    , unlinesPS
    , hashPS
    , breakFirstPS
    , breakLastPS
    , substrPS
    , isFunky
    , fromHex2PS
    , fromPS2Hex
    , betweenLinesPS
    , intercalate
    -- * encoding and unicode utilities
    , isAscii
    , decodeLocale
    , encodeLocale
    , unpackPSFromUTF8
    , packStringToUTF8
    -- * properties
    , prop_unlinesPS_linesPS_left_inverse
    , prop_linesPS_length
    , prop_unlinesPS_length
    , propHexConversion
    , spec_betweenLinesPS
    ) where

import Darcs.Prelude

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import Data.ByteString (intercalate)
import qualified Data.ByteString.Base16     as B16

import System.IO ( withFile, IOMode(ReadMode)
                 , hSeek, SeekMode(SeekFromEnd,AbsoluteSeek)
                 , openBinaryFile, hClose, Handle, hGetChar
                 , stdin)
import System.IO.Error          ( catchIOError )
import System.IO.Unsafe         ( unsafePerformIO )

import Data.Bits                ( rotateL )
import Data.Char                ( ord )
import Data.Word                ( Word8 )
import Data.Int                 ( Int32, Int64 )
import Data.List                ( intersperse )
import Control.Exception ( throw )
import Control.Monad            ( when )
import Control.Monad.ST.Lazy    ( ST )

import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib.Internal as ZI
import Darcs.Util.Encoding ( decode, encode, decodeUtf8, encodeUtf8 )
import Darcs.Util.Global ( addCRCWarning )

#if mingw32_HOST_OS
#else
import System.IO.MMap( mmapFileByteString )
#endif
import System.Mem( performGC )
import System.Posix.Files( fileSize, getSymbolicLinkStatus )

------------------------------------------------------------------------
-- A locale-independent isspace(3) so patches are interpreted the same everywhere.
-- ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 0x20 = True
isSpaceWord8 0x09 = True
isSpaceWord8 0x0A = True
isSpaceWord8 0x0D = True
isSpaceWord8 _    = False
{-# INLINE isSpaceWord8 #-}

-- | Drop leading white space, where white space is defined as
-- consisting of ' ', '\t', '\n', or '\r'.
dropSpace :: B.ByteString -> B.ByteString
dropSpace bs = B.dropWhile isSpaceWord8 bs

------------------------------------------------------------------------

{-# INLINE isFunky #-}
isFunky :: B.ByteString -> Bool
isFunky ps = 0 `B.elem` ps || 26 `B.elem` ps

------------------------------------------------------------------------

{-# INLINE hashPS #-}
hashPS :: B.ByteString -> Int32
hashPS = B.foldl' hashByte 0

{-# INLINE hashByte #-}
hashByte :: Int32 -> Word8 -> Int32
hashByte h x = fromIntegral x + rotateL h 8

{-# INLINE substrPS #-}
substrPS :: B.ByteString -> B.ByteString -> Maybe Int
substrPS tok str
    | B.null tok = Just 0
    | B.length tok > B.length str = Nothing
    | otherwise = do n <- B.elemIndex (B.head tok) str
                     let ttok = B.tail tok
                         reststr = B.drop (n+1) str
                     if ttok == B.take (B.length ttok) reststr
                        then Just n
                        else ((n+1)+) `fmap` substrPS tok reststr

------------------------------------------------------------------------

-- TODO: replace breakFirstPS and breakLastPS with definitions based on
-- ByteString's break/breakEnd
{-# INLINE breakFirstPS #-}
breakFirstPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakFirstPS c p = case BC.elemIndex c p of
                   Nothing -> Nothing
                   Just n -> Just (B.take n p, B.drop (n+1) p)

{-# INLINE breakLastPS #-}
breakLastPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakLastPS c p = case BC.elemIndexEnd c p of
                  Nothing -> Nothing
                  Just n -> Just (B.take n p, B.drop (n+1) p)

------------------------------------------------------------------------
-- linesPS and unlinesPS

{-# INLINE linesPS #-}
linesPS :: B.ByteString -> [B.ByteString]
linesPS ps
     | B.null ps = [B.empty]
     | otherwise = BC.split '\n' ps

{-# INLINE unlinesPS #-}
unlinesPS :: [B.ByteString] -> B.ByteString
unlinesPS [] = B.empty
unlinesPS x  = B.concat $ intersperse (BC.singleton '\n') x

-- properties of linesPS and unlinesPS

prop_unlinesPS_linesPS_left_inverse :: B.ByteString -> Bool
prop_unlinesPS_linesPS_left_inverse x = unlinesPS (linesPS x) == x

prop_linesPS_length :: B.ByteString -> Bool
prop_linesPS_length x = length (linesPS x) == length (BC.elemIndices '\n' x) + 1

prop_unlinesPS_length :: [B.ByteString] -> Bool
prop_unlinesPS_length xs =
  B.length (unlinesPS xs) == if null xs then 0 else sum (map B.length xs) + length xs - 1

-- -----------------------------------------------------------------------------
-- gzReadFilePS

-- |Decompress the given bytestring into a lazy list of chunks, along with a boolean
-- flag indicating (if True) that the CRC was corrupted.
-- Inspecting the flag will cause the entire list of chunks to be evaluated (but if
-- you throw away the list immediately this should run in constant space).
gzDecompress :: Maybe Int -> BL.ByteString -> ([B.ByteString], Bool)
gzDecompress mbufsize =
    -- This is what the code would be without the bad CRC recovery logic:
    -- return . BL.toChunks . GZ.decompressWith decompressParams
    decompressWarn (ZI.decompressST ZI.gzipFormat decompressParams)
  where
        decompressParams = case mbufsize of
                              Just bufsize -> GZ.defaultDecompressParams { GZ.decompressBufferSize = bufsize }
                              Nothing -> GZ.defaultDecompressParams

        decompressWarn :: (forall s . ZI.DecompressStream (ST s)) -> BL.ByteString -> ([B.ByteString], Bool)
        decompressWarn = ZI.foldDecompressStreamWithInput
                           (\x ~(xs, b) -> (x:xs, b))
                           (\xs -> if BL.null xs
                                      then ([], False)
                                      else error "trailing data at end of compressed stream"
                           )
                           handleBad

        -- For a while a bug in darcs caused gzip files with good data but bad CRCs to be
        -- produced. Trap bad CRC messages, run the specified action to report that it happened,
        -- but continue on the assumption that the data is valid.
        handleBad (ZI.DataFormatError "incorrect data check") = ([], True)
        handleBad e = error (show e)

isGZFile :: FilePath -> IO (Maybe Int)
isGZFile f = do
    h <- openBinaryFile f ReadMode
    header <- B.hGet h 2
    if header /= B.pack [31,139]
       then do hClose h
               return Nothing
       else do hSeek h SeekFromEnd (-4)
               len <- hGetLittleEndInt h
               hClose h
               return (Just len)

-- | Read an entire file, which may or may not be gzip compressed, directly
-- into a 'B.ByteString'.
gzReadFilePS :: FilePath -> IO B.ByteString
gzReadFilePS f = do
    mlen <- isGZFile f
    case mlen of
       Nothing -> mmapFilePS f
       Just len ->
            do -- Passing the length to gzDecompress means that it produces produces one chunk,
               -- which in turn means that B.concat won't need to copy data.
               -- If the length is wrong this will just affect efficiency, not correctness
               let doDecompress buf = let (res, bad) = gzDecompress (Just len) buf
                                      in do when bad $ addCRCWarning f
                                            return res
               compressed <- (BL.fromChunks . return) `fmap` mmapFilePS f
               B.concat `fmap` doDecompress compressed

hGetLittleEndInt :: Handle -> IO Int
hGetLittleEndInt h = do
    b1 <- ord `fmap` hGetChar h
    b2 <- ord `fmap` hGetChar h
    b3 <- ord `fmap` hGetChar h
    b4 <- ord `fmap` hGetChar h
    return $ b1 + 256*b2 + 65536*b3 + 16777216*b4

gzWriteFilePS :: FilePath -> B.ByteString -> IO ()
gzWriteFilePS f ps = gzWriteFilePSs f [ps]

gzWriteFilePSs :: FilePath -> [B.ByteString] -> IO ()
gzWriteFilePSs f pss  =
    BL.writeFile f $ GZ.compress $ BL.fromChunks pss

gzWriteHandle :: Handle -> [B.ByteString] -> IO ()
gzWriteHandle h pss  =
    BL.hPut h $ GZ.compress $ BL.fromChunks pss

-- | Read standard input, which may or may not be gzip compressed, directly
-- into a 'B.ByteString'.
gzReadStdin :: IO B.ByteString
gzReadStdin = do
    header <- B.hGet stdin 2
    rest   <- B.hGetContents stdin
    let allStdin = B.concat [header,rest]
    return $
     if header /= B.pack [31,139]
      then allStdin
      else let decompress = fst . gzDecompress Nothing
               compressed = BL.fromChunks [allStdin]
           in
           B.concat $ decompress compressed

-- | Pointer to a filesystem, possibly with start/end offsets. Supposed to be
-- fed to (uncurry mmapFileByteString) or similar.
type FileSegment = (FilePath, Maybe (Int64, Int))

-- | Read in a FileSegment into a Lazy ByteString. Implemented using mmap.
readSegment :: FileSegment -> IO BL.ByteString
readSegment (f,range) = do
    bs <- tryToRead
       `catchIOError` (\_ -> do
                     size <- fileSize `fmap` getSymbolicLinkStatus f
                     if size == 0
                        then return B.empty
                        else performGC >> tryToRead)
    return $ BL.fromChunks [bs]
  where
    tryToRead =
        case range of
            Nothing -> B.readFile f
            Just (off, size) -> withFile f ReadMode $ \h -> do
                hSeek h AbsoluteSeek $ fromIntegral off
                B.hGet h size
{-# INLINE readSegment #-}

-- -----------------------------------------------------------------------------
-- mmapFilePS

-- | Like readFilePS, this reads an entire file directly into a
-- 'B.ByteString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents of
-- the file never need to be copied.  Also, under memory pressure the page
-- may simply be discarded, wile in the case of readFilePS it would need to
-- be written to swap.  If you read many small files, mmapFilePS will be
-- less memory-efficient than readFilePS, since each mmapFilePS takes up a
-- separate page of memory.  Also, you can run into bus errors if the file
-- is modified.

mmapFilePS :: FilePath -> IO B.ByteString
#if mingw32_HOST_OS
mmapFilePS = B.readFile
#else
mmapFilePS f =
  mmapFileByteString f Nothing
   `catchIOError` (\_ -> do
                     size <- fileSize `fmap` getSymbolicLinkStatus f
                     if size == 0
                        then return B.empty
                        else performGC >> mmapFileByteString f Nothing)
#endif

-- -------------------------------------------------------------------------
-- fromPS2Hex

fromPS2Hex :: B.ByteString -> B.ByteString
fromPS2Hex = B16.encode

-- -------------------------------------------------------------------------
-- fromHex2PS

fromHex2PS :: B.ByteString -> B.ByteString
fromHex2PS s =
  case B16.decode s of
    Right result -> result
    Left msg -> throw $ userError $ "fromHex2PS: input is not hex encoded: "++msg

propHexConversion :: B.ByteString -> Bool
propHexConversion x = fromHex2PS (fromPS2Hex x) == x

-- -------------------------------------------------------------------------
-- betweenLinesPS

-- | Return the B.ByteString between the two lines given,
-- or Nothing if they do not appear.
betweenLinesPS :: B.ByteString -> B.ByteString -> B.ByteString
               -> Maybe B.ByteString
betweenLinesPS start end ps =
  case B.breakSubstring start_line ps of
    (before_start, at_start)
      | not (B.null at_start)
      , B.null before_start || BC.last before_start == '\n' ->
          case B.breakSubstring end_line (B.drop (B.length start_line) at_start) of
            (before_end, at_end)
              | not (B.null at_end)
              , B.null before_end || BC.last before_end == '\n' -> Just before_end
              | otherwise -> Nothing
      | otherwise -> Nothing
  where
    start_line = BC.snoc start '\n'
    end_line = BC.snoc end '\n'

-- | Simpler but less efficient variant of 'betweenLinesPS'.
spec_betweenLinesPS :: B.ByteString -> B.ByteString -> B.ByteString
                    -> Maybe B.ByteString
spec_betweenLinesPS start end ps =
  case break (start ==) (linesPS ps) of
    (_, _:after_start) ->
      case break (end ==) after_start of
        (before_end, _:_) ->
          Just $ BC.unlines before_end
        _ -> Nothing
    _ -> Nothing

-- | Test if a ByteString is made of ascii characters
isAscii :: B.ByteString -> Bool
isAscii = B.all (< 128)

-- * Encoding functions

-- Use of 'unsafePerformIO' is ratified by the fact that these
-- really are pure functions.

-- | Decode a 'ByteString' containing UTF-8 to a 'String'. Decoding errors
-- are flagged with the U+FFFD character.
unpackPSFromUTF8 :: B.ByteString -> String
unpackPSFromUTF8  = unsafePerformIO . decodeUtf8

-- | Encode a 'String' to a 'ByteString' using UTF-8.
packStringToUTF8 :: String -> B.ByteString
packStringToUTF8 = unsafePerformIO . encodeUtf8

-- | Decode a 'ByteString' to a 'String' according to the current locale,
-- using lone surrogates for un-decodable bytes.
decodeLocale :: B.ByteString -> String
decodeLocale = unsafePerformIO . decode

-- | Encode a 'String' to a 'ByteString' according to the current locale,
-- converting lone surrogates back to the original byte. If that
-- fails (because the locale does not support the full unicode range)
-- then encode using utf-8, assuming that the un-ecodable characters
-- come from patch meta data.
--
-- See also 'Darcs.UI.Commands.setEnvCautiously'.
encodeLocale :: String -> B.ByteString
encodeLocale s = unsafePerformIO $ encode s `catchIOError` (\_ -> encodeUtf8 s)
