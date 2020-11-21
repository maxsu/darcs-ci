{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.NamedPrim ( aPatchId ) where

import Prelude ()
import Darcs.Prelude
import Test.QuickCheck
import Test.QuickCheck.Gen ( chooseAny )

import Darcs.Patch.Prim.Named ( NamedPrim, namedPrim, PrimPatchId, unsafePrimPatchId )
import Darcs.Patch.Prim.WithName ( PrimWithName(..), wnPatch )
import Darcs.Util.Hash ( SHA1(..) )

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Shrink

import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.TestOnly.Instance ()

import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed

type instance ModelOf (NamedPrim p) = ModelOf p
instance
    ( RepoModel (ModelOf p)
    , ArbitraryState (NamedPrim p)
    , ArbitraryPrim p
    )
  => ArbitraryPrim (NamedPrim p) where
    runCoalesceTests = Nothing
    hasPrimConstruct = Nothing
    usesV1Model = Nothing

instance Shrinkable prim => Shrinkable (PrimWithName n prim) where
  shrinkInternally (PrimWithName n p) = PrimWithName n <$> shrinkInternally p
  shrinkAtEnd (PrimWithName n p) = mapSeal (PrimWithName n) <$> shrinkAtEnd p
  shrinkAtStart (PrimWithName n p) = mapFlipped (PrimWithName n) <$> shrinkAtStart p

instance MightBeEmptyHunk p => MightBeEmptyHunk (NamedPrim p) where
  isEmptyHunk = isEmptyHunk . wnPatch

instance MightHaveDuplicate (NamedPrim p)

instance NullPatch p => NullPatch (NamedPrim p) where
  nullPatch p = nullPatch (wnPatch p)

instance ArbitraryState prim => ArbitraryState (NamedPrim prim) where
  arbitraryState repo = do
    Sealed (WithEndState p repo') <- arbitraryState repo
    pid <- aPatchId
    return $ Sealed $ WithEndState (namedPrim pid p) repo'


instance PropagateShrink prim1 prim2 => PropagateShrink prim1 (PrimWithName n2 prim2) where
  propagateShrink (p1 :> PrimWithName n2 p2) = do
    mp2' :> mp1' <- propagateShrink (p1 :> p2)
    return (mapMB_MB (PrimWithName n2) mp2' :> mp1')

aPatchId :: Gen PrimPatchId
aPatchId = unsafePrimPatchId <$> (arbitrarySizedNatural `suchThat` (> 0)) <*> aHash

aHash :: Gen SHA1
aHash =
  -- it's important to avoid hash collisions, so we use chooseAny rather
  -- than arbitrary so that the values generated are uniformly distributed
  SHA1 <$> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny
