{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Read () where

import Darcs.Prelude

import Darcs.Patch.Invert ( invert )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Util.Parser ( Parser, choice, string,
                                lexChar, lexWord, skipSpace )

import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import Darcs.Patch.V1.Commute ( merger )

import Darcs.Patch.Witnesses.Sealed ( Sealed(..), seal, mapSeal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

import Control.Monad ( liftM )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.ByteString       as B  (ByteString )


instance PrimPatch prim => ReadPatch (RepoPatchV1 prim) where
 readPatch'
   = choice [ liftM seal $ skipSpace >> readMerger True
            , liftM seal $ skipSpace >> readMerger False
            , liftM (mapSeal PP) readPatch'
            ]
readMerger :: (PrimPatch prim) => Bool -> Parser (RepoPatchV1 prim wX wY)
readMerger b = do string s
                  g <- lexWord
                  lexChar '('
                  Sealed p1 <- readPatch'
                  Sealed p2 <- readPatch'
                  lexChar ')'
                  Sealed m <- return $ merger (BC.unpack g) p1 p2
                  return $ if b then unsafeCoerceP m else unsafeCoerceP (invert m)
  where
  s | b         = merger'
    | otherwise = regrem

merger' :: B.ByteString
merger' = BC.pack "merger"

regrem :: B.ByteString
regrem = BC.pack "regrem"

