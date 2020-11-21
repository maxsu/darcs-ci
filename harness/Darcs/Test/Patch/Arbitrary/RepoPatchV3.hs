{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
module Darcs.Test.Patch.Arbitrary.RepoPatchV3 () where

import Darcs.Prelude

import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate(..), PrimBased(..), ArbitraryPrim )
import Darcs.Test.Patch.Arbitrary.NamedPrim ()
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel ( RepoState, ModelOf )
import Darcs.Test.Patch.WithState ( PropagateShrink )

import Darcs.Patch
import Darcs.Patch.Annotate
import Darcs.Patch.Prim.Named
import Darcs.Patch.Prim.WithName
import Darcs.Patch.V3 ( RepoPatchV3 )
import qualified Darcs.Patch.V3.Core as V3 ( RepoPatchV3(Prim) )

import Darcs.Patch.Witnesses.Ordered

instance MightHaveDuplicate (RepoPatchV3 prim) where
  hasDuplicate _ = False

type instance ModelOf (RepoPatchV3 prim) = ModelOf prim

instance
  (Annotate prim, ArbitraryPrim prim, PrimPatch prim, ApplyState prim ~ RepoState (ModelOf prim))
  => ArbitraryRepoPatch (RepoPatchV3 prim)
  where

    notRepoPatchV1 = Just (NotRepoPatchV1 (\case {}))

instance PrimPatch prim => CheckedMerge (RepoPatchV3 prim)

instance (PrimPatch prim, ArbitraryPrim prim, PropagateShrink prim prim) => PrimBased (RepoPatchV3 prim) where
  type OnlyPrim (RepoPatchV3 prim) = NamedPrim prim
  primEffect p = wnPatch p :>: NilFL
  liftFromPrim = V3.Prim
