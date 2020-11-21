{-# LANGUAGE OverloadedStrings #-}
module Darcs.Test.Patch.Arbitrary.NamedPrimV1 () where

import Prelude ()
import Darcs.Prelude

import Test.QuickCheck

import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Prim.Named ( NamedPrim, namedPrim )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim(..) )

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.Arbitrary.NamedPrim ( aPatchId )
import qualified Darcs.Test.Patch.Arbitrary.PrimV1 as V1
import Darcs.Test.Patch.V1Model

type Prim = NamedPrim V2.Prim

----------------------------------------------------------------------
-- Arbitrary instances

aPrimPair :: V1Model wX
          -> Gen (WithEndState V1Model ((Prim :> Prim) wX) wY)
aPrimPair repo = do
  WithEndState (p1:>p2) repo' <- V1.aPrimPair repo
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
