{-# LANGUAGE OverloadedStrings #-}
module Darcs.Test.Patch.Arbitrary.NamedPrimFileUUID () where

import Prelude ()
import Darcs.Prelude

import Test.QuickCheck

import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Prim.Named ( NamedPrim, namedPrim )
import qualified Darcs.Patch.Prim.FileUUID as FileUUID

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.Arbitrary.NamedPrim ( aPatchId )
import qualified Darcs.Test.Patch.Arbitrary.PrimFileUUID as FileUUID
import Darcs.Test.Patch.FileUUIDModel

type Prim = NamedPrim FileUUID.Prim

----------------------------------------------------------------------
-- Arbitrary instances

aPrimPair :: FileUUIDModel wX
          -> Gen (WithEndState FileUUIDModel ((Prim :> Prim) wX) wY)
aPrimPair repo = do
  WithEndState (p1:>p2) repo' <- FileUUID.aPrimPair repo
  pid1 <- aPatchId
  pid2 <- aPatchId
  return $ WithEndState (namedPrim pid1 p1 :> namedPrim pid2 p2) repo'

-- use the special generator for pairs
arbitraryPair :: Gen (Sealed2 (WithState (Prim :> Prim)))
arbitraryPair = do
  repo <- aSmallRepo
  WithEndState pp repo' <- aPrimPair repo
  return $ seal2 $ WithState repo pp repo'

instance Arbitrary (Sealed2 Prim) where
  arbitrary = makeS2Gen aSmallRepo

instance Arbitrary (Sealed2 (Prim :> Prim)) where
  arbitrary = mapSeal2 wsPatch <$> arbitraryPair

instance Arbitrary (Sealed2 (WithState Prim)) where
  arbitrary = makeWS2Gen aSmallRepo

instance Arbitrary (Sealed2 (WithState (Prim :> Prim))) where
  arbitrary = arbitraryPair
