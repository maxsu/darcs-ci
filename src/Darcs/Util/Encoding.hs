-- Copyright 2007-2009, Judah Jacobson.
-- All Rights Reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- - Redistribution of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.

-- - Redistribution in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE CPP #-}
module Darcs.Util.Encoding
    ( encode, decode
    , encodeUtf8, decodeUtf8
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B
import GHC.IO.Encoding ( TextEncoding, mkTextEncoding )
import GHC.Foreign ( withCStringLen, peekCStringLen )

#ifdef WIN32
import Darcs.Util.Encoding.Win32 ( encode, decode )
#else

import GHC.IO.Encoding ( getFileSystemEncoding )

-- | Encode a 'String' into a 'B.ByteString' according to the user's locale
-- with the ghc specific //ROUNDTRIP feature added. This means the argument
-- is allowed to contain non-Unicode 'Char's as produced by 'decode'.
encode :: String -> IO B.ByteString
encode s = getFileSystemEncoding >>= textEncode s

-- | Decode a 'B.ByteString' into a 'String' according to the user's locale
-- with the ghc specific //ROUNDTRIP feature added. This means the result
-- may contain 'Char's that are not valid Unicode in case decoding with the
-- user's locale fails.
decode :: B.ByteString -> IO String
decode bs = getFileSystemEncoding >>= textDecode bs

#endif

encodeUtf8 :: String -> IO B.ByteString
encodeUtf8 s = mkTextEncoding "UTF-8//TRANSLIT" >>= textEncode s

decodeUtf8 :: B.ByteString -> IO String
decodeUtf8 bs = mkTextEncoding "UTF-8//TRANSLIT" >>= textDecode bs

textEncode :: String -> TextEncoding -> IO B.ByteString
textEncode s enc = withCStringLen enc s B.packCStringLen

textDecode :: B.ByteString -> TextEncoding -> IO String
textDecode bs enc = B.useAsCStringLen bs (peekCStringLen enc)

