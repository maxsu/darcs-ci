{-# LANGUAGE UndecidableInstances, ViewPatterns #-}
-- | Test case generator for patch with a Merge instance
module Darcs.Test.Patch.Arbitrary.RepoPatch
  ( withSingle
  , withPair
  , withTriple
  , withFork
  , withSequence
  , withAllSequenceItems
  , NotRepoPatchV1(..)
  , ArbitraryRepoPatch(..)
  ) where

import Darcs.Prelude

import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Arbitrary.Generic
  ( mergeableSequenceToRL, MergeableSequence(..),  ArbitraryPrim(..), PrimBased )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Ordered hiding ( Fork )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.V1 ( RepoPatchV1 )

import Data.Constraint
import Data.Void

data NotRepoPatchV1 p = NotRepoPatchV1 (forall prim . Dict (p ~ RepoPatchV1 prim) -> Void)

-- | Class to simplify type signatures and superclass constraints.
class
  ( RepoPatch p
  , ArbitraryPrim (PrimOf p)
  , ModelOf p ~ ModelOf (PrimOf p)
  , ApplyState p ~ RepoState (ModelOf p)
  ) => ArbitraryRepoPatch p where

  notRepoPatchV1 :: Maybe (NotRepoPatchV1 p)


withSingle
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withSingle prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp -> Just (prop pp)
      _ -> Nothing

withPair
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (p :> p) wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withPair prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp1 :<: pp2 -> Just (prop (pp1 :> pp2))
      _ -> Nothing

withTriple
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (p :> p :> p) wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
withTriple prop (Sealed2 (WithStartState2 _ ms))
  = case mergeableSequenceToRL ms of
      _ :<: pp1 :<: pp2 :<: pp3 -> Just (prop (pp1 :> pp2 :> pp3))
      _ -> Nothing

withFork
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. (FL p :\/: FL p) wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> Maybe r
-- We can't use (MergeableSequence p:\/: MergeableSequence p) as the input because
-- the witnesses would be wrong, so just use MergeableSequence p and choose the
-- ParMS cases.
withFork prop (Sealed2 (WithStartState2 _ (ParMS ms1 ms2)))
  = Just (prop (reverseRL (mergeableSequenceToRL ms1) :\/: reverseRL (mergeableSequenceToRL ms2)))
withFork _ _ = Nothing

withSequence
  :: (CheckedMerge p, PrimBased p)
  => (forall wX wY. RL p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> r
withSequence prop (Sealed2 (WithStartState2 _ ms))
  = prop (mergeableSequenceToRL ms)

withAllSequenceItems
  :: (CheckedMerge p, PrimBased p, Monoid r)
  => (forall wX wY. p wX wY -> r)
  -> Sealed2 (WithStartState2 (MergeableSequence p)) -> r
withAllSequenceItems prop (Sealed2 (WithStartState2 _ ms))
  = mconcat . mapRL prop . mergeableSequenceToRL $ ms

