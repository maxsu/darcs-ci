{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.FileUUID.Read () where

import Darcs.Prelude hiding ( take )

import Control.Monad ( liftM, liftM2 )

import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Prim.Class( PrimRead(..) )
import Darcs.Patch.Prim.FileUUID.Core( Prim(..), Hunk(..) )
import Darcs.Patch.Prim.FileUUID.ObjectMap
import Darcs.Patch.Witnesses.Sealed( seal )

import Darcs.Util.Path ( decodeWhiteName )
import Darcs.Util.Parser

instance PrimRead Prim where
  readPrim _ = do
    skipSpace
    choice $ map (liftM seal)
      [ identity
      , hunk "hunk" Hunk
      , manifest "manifest" Manifest
      , manifest "demanifest" Demanifest
      ]
    where
      manifest kind ctor = liftM2 ctor (patch kind) location
      identity = lexString "identity" >> return Identity
      patch x = string x >> uuid
      uuid = UUID <$> lexWord
      filename = decodeWhiteName <$> lexWord
      content = do
        lexString "content"
        len <- int
        _ <- char '\n'
        take len
      location = liftM2 L uuid filename
      hunk kind ctor = do
        uid <- patch kind
        offset <- int
        old <- content
        new <- content
        return $ ctor uid (H offset old new)

instance ReadPatch Prim where
  readPatch' = readPrim undefined
