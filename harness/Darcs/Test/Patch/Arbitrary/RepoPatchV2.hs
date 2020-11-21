{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.RepoPatchV2 () where

import Darcs.Prelude

import Control.Exception
import System.IO.Unsafe

import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate(..), PrimBased(..), ArbitraryPrim )
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge(..) )
import Darcs.Test.Patch.RepoModel ( RepoState, ModelOf )
import Darcs.Test.Patch.WithState ( PropagateShrink )
import Darcs.Patch
import Darcs.Patch.Annotate
import Darcs.Patch.V2 ( RepoPatchV2 )
import Darcs.Patch.V2.RepoPatch ( isDuplicate, RepoPatchV2(Normal) )
import Darcs.Patch.Witnesses.Ordered

instance MightHaveDuplicate (RepoPatchV2 prim) where
  hasDuplicate = isDuplicate

type instance ModelOf (RepoPatchV2 prim) = ModelOf prim

instance
  (Annotate prim, ArbitraryPrim prim, PrimPatch prim, ApplyState prim ~ RepoState (ModelOf prim))
  => ArbitraryRepoPatch (RepoPatchV2 prim)
  where

    notRepoPatchV1 = Just (NotRepoPatchV1 (\case {}))

instance PrimPatch prim => CheckedMerge (RepoPatchV2 prim) where
  validateMerge v =
    case unsafePerformIO (try (evaluate v)) of
      Left (_ :: SomeException) -> Nothing
      Right x -> Just x

instance (PrimPatch prim, ArbitraryPrim prim, PropagateShrink prim prim) => PrimBased (RepoPatchV2 prim) where
  type OnlyPrim (RepoPatchV2 prim) = prim
  primEffect = (:>: NilFL)
  liftFromPrim = Normal
