-- UndecidableInstances was added because GHC 8.6 needed it
-- even though GHC 8.2 didn't
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Darcs.Test.Patch.Arbitrary.PatchTree
  ( Tree(..)
  , TreeWithFlattenPos(..)
  , G2(..)
  , flattenOne
  , flattenTree
  , mapTree
  , commutePairFromTree
  , mergePairFromTree
  , commuteTripleFromTree
  , mergePairFromCommutePair
  , commutePairFromTWFP
  , mergePairFromTWFP
  , getPairs
  , getTriples
  , patchFromTree
  , canonizeTree
  ) where

import Darcs.Prelude

import Test.QuickCheck

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Util.QuickCheck ( bSized )

import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FromPrim ( FromPrim(..), PrimOf )
import Darcs.Patch.Witnesses.Show

-- | A 'Tree' of patches 'p' starting at state 'wX' simulating
-- several branches of a repo. The end states of the branches
-- may of course differ.
data Tree p wX where
   NilTree :: Tree p wX
   SeqTree :: p wX wY -> Tree p wY -> Tree p wX
   ParTree :: Tree p wX -> Tree p wX -> Tree p wX

mapTree :: (forall wY wZ . p wY wZ -> q wY wZ) -> Tree p wX -> Tree q wX
mapTree _ NilTree = NilTree
mapTree f (SeqTree p t) = SeqTree (f p) (mapTree f t)
mapTree f (ParTree t1 t2) = ParTree (mapTree f t1) (mapTree f t2)

instance Show2 p => Show (Tree p wX) where
   showsPrec _ NilTree = showString "NilTree"
   showsPrec d (SeqTree a t) = showParen (d > appPrec) $ showString "SeqTree " .
                               showsPrec2 (appPrec + 1) a . showString " " .
                               showsPrec (appPrec + 1) t
   showsPrec d (ParTree t1 t2) = showParen (d > appPrec) $ showString "ParTree " .
                                 showsPrec (appPrec + 1) t1 . showString " " .
                                 showsPrec (appPrec + 1) t2

instance Show2 p => Show1 (Tree p)

instance Show2 p => Show1 (TreeWithFlattenPos p)

-- | The number of patches in a 'Tree'. This is the (common) length of all
-- elements of 'flattenTree'.
sizeTree :: Tree p wX -> Int
sizeTree NilTree = 0
sizeTree (SeqTree _ t) = 1 + sizeTree t
sizeTree (ParTree t1 t2) = sizeTree t1 + sizeTree t2

-- | The number of successive pairs in a flattened 'Tree'.
numPairs :: Tree p wX -> Int
numPairs t =
  case sizeTree t of
    0 -> 0
    s -> s - 1

-- | The number of successive triples in a flattened 'Tree'.
numTriples :: Tree p wX -> Int
numTriples t =
  case sizeTree t of
    0 -> 0
    1 -> 0
    s -> s - 2

newtype G2 l p wX wY = G2 { unG2 :: l (p wX wY) }

-- | All possible ways that the several branches of a 'Tree' can be
-- merged into a linear sequence.
flattenTree :: (Merge p) => Tree p wZ -> Sealed (G2 [] (FL p) wZ)
flattenTree NilTree = seal $ G2 $ return NilFL
flattenTree (SeqTree p t) = mapSeal (G2 . map (p :>:) . unG2) $ flattenTree t
flattenTree (ParTree (flattenTree -> Sealed gpss1) (flattenTree -> Sealed gpss2)) =
  seal $
  G2 $ do
    ps1 <- unG2 gpss1
    ps2 <- unG2 gpss2
    ps2' :/\: ps1' <- return $ merge (ps1 :\/: ps2)
    -- We can't prove that the existential type in the result
    -- of merge will be the same for each pair of ps1 and ps2.
    map unsafeCoerceP [ps1 +>+ ps2', ps2 +>+ ps1']

-- | Generate a tree of patches, bounded by depth.
arbitraryTree :: ArbitraryState p => ModelOf p wX -> Int -> Gen (Tree p wX)
arbitraryTree rm depth
  | depth == 0 = return NilTree
    -- Note a probability of N for NilTree would imply ~(100*N)% of empty trees.
    -- For the purpose of this module empty trees are useless, but even when
    -- NilTree case is omitted there is still a small percentage of empty trees
    -- due to the generation of null-patches (empty-hunks) and the use of canonizeTree.
  | otherwise =
    frequency
      [ ( 1
        , do Sealed (WithEndState p rm') <- arbitraryState rm
             t <- arbitraryTree rm' (depth - 1)
             return (SeqTree p t))
      , ( 3
        , do t1 <- arbitraryTree rm (depth - 1)
             t2 <- arbitraryTree rm (depth - 1)
             return (ParTree t1 t2))
      ]

-- | Canonize a 'Tree', removing any dead branches.
canonizeTree :: NullPatch p => Tree p wX -> Tree p wX
canonizeTree NilTree = NilTree
canonizeTree (ParTree t1 t2)
    | NilTree <- canonizeTree t1 = canonizeTree t2
    | NilTree <- canonizeTree t2 = canonizeTree t1
    | otherwise = ParTree (canonizeTree t1) (canonizeTree t2)
canonizeTree (SeqTree p t) | IsEq <- nullPatch p = canonizeTree t
                           | otherwise = SeqTree p (canonizeTree t)


-- | Generate a patch to a certain state.
class ArbitraryStateIn s p where
  arbitraryStateIn :: s wX -> Gen (p wX)

instance (ArbitraryState p, s ~ ModelOf p) => ArbitraryStateIn s (Tree p) where
  -- Don't generate trees deeper than 6 with default QuickCheck size (0..99).
  -- Note if we don't put a non-zero lower bound the first generated trees will
  -- always have depth 0.
  -- The minimum size of 3 means that we have a reasonable probability that the
  -- Tree has at least one triple.
  arbitraryStateIn rm = bSized 3 0.035 9 $ \depth -> arbitraryTree rm depth

instance ( RepoModel model
         , ArbitraryPrim prim
         , model ~ ModelOf prim
         , ArbitraryState prim
         ) =>
         Arbitrary (Sealed (WithStartState model (Tree prim))) where
  arbitrary = do
    repo <- aSmallRepo
    Sealed . WithStartState repo <$>
      (canonizeTree <$> arbitraryStateIn repo) `suchThat` (\t -> numTriples t >= 1)

flattenOne :: (FromPrim p, Merge p) => Tree (PrimOf p) wX -> Sealed (FL p wX)
flattenOne NilTree = seal NilFL
flattenOne (SeqTree p (flattenOne -> Sealed ps)) = seal (fromAnonymousPrim p :>: ps)
flattenOne (ParTree (flattenOne -> Sealed ps1) (flattenOne -> Sealed ps2)) =
    case merge (ps1 :\/: ps2) of
      ps2' :/\: _ -> seal (ps1 +>+ ps2')

-- | A 'Tree' together with some number that is no greater than
-- the number of pairs in the 'Tree'.
data TreeWithFlattenPos p wX = TWFP Int (Tree p wX)

commutePairFromTWFP :: (FromPrim p, Merge p)
                    => (forall wY wZ . (p :> p) wY wZ -> t)
                    -> Sealed (WithStartState model (TreeWithFlattenPos (PrimOf p)))
                    -> Maybe t
commutePairFromTWFP handlePair (Sealed (WithStartState _ (TWFP n t)))
    = unseal2 handlePair <$>
      let xs = unseal getPairs (flattenOne t)
      in if length xs > n && n >= 0 then Just (xs!!n) else Nothing

commutePairFromTree :: (FromPrim p, Merge p)
                    => (forall wY wZ . (p :> p) wY wZ -> t)
                    -> Sealed (WithStartState model (Tree (PrimOf p)))
                    -> Maybe t
commutePairFromTree handlePair (Sealed (WithStartState _ t))
   = unseal2 handlePair <$>
     let xs = unseal getPairs (flattenOne t)
     in if null xs then Nothing else Just (last xs)

commuteTripleFromTree :: (FromPrim p, Merge p)
                      => (forall wY wZ . (p :> p :> p) wY wZ -> t)
                      -> Sealed (WithStartState model (Tree (PrimOf p)))
                      -> Maybe t
commuteTripleFromTree handle (Sealed (WithStartState _ t))
   = unseal2 handle <$>
     case flattenOne t of
       Sealed ps ->
         let xs = getTriples ps
         in if null xs
            then Nothing
            else Just (last xs)

mergePairFromCommutePair :: Commute p
                         => (forall wY wZ . (p :\/: p) wY wZ -> t)
                         -> (forall wY wZ . (p :>   p) wY wZ -> t)
mergePairFromCommutePair handlePair (a :> b)
 = case commute (a :> b) of
     Just (b' :> _) -> handlePair (a :\/: b')
     Nothing -> handlePair (b :\/: b)

-- impredicativity problems mean we can't use (.) in the definitions below

mergePairFromTWFP :: (FromPrim p, Commute p, Merge p)
                  => (forall wY wZ . (p :\/: p) wY wZ -> t)
                  -> Sealed (WithStartState model (TreeWithFlattenPos (PrimOf p)))
                  -> Maybe t
mergePairFromTWFP x = commutePairFromTWFP (mergePairFromCommutePair x)

mergePairFromTree :: (FromPrim p, Commute p, Merge p)
                  => (forall wY wZ . (p :\/: p) wY wZ -> t)
                  -> Sealed (WithStartState model (Tree (PrimOf p)))
                  -> Maybe t
mergePairFromTree x = commutePairFromTree (mergePairFromCommutePair x)

patchFromCommutePair :: (forall wY wZ . p wY wZ -> t)
                     -> (forall wY wZ . (p :> p) wY wZ -> t)
patchFromCommutePair handle (_ :> b) = handle b

patchFromTree :: (FromPrim p, Merge p)
              => (forall wY wZ . p wY wZ -> t)
              -> Sealed (WithStartState model (Tree (PrimOf p)))
              -> Maybe t
patchFromTree x = commutePairFromTree (patchFromCommutePair x)


instance Show2 p => Show (TreeWithFlattenPos p wX) where
   showsPrec d (TWFP n t) = showParen (d > appPrec) $ showString "TWFP " .
                            showsPrec (appPrec + 1) n . showString " " .
                            showsPrec1 (appPrec + 1) t

getPairs :: FL p wX wY -> [Sealed2 (p :> p)]
getPairs NilFL = []
getPairs (_:>:NilFL) = []
getPairs (a:>:b:>:c) = seal2 (a:>b) : getPairs (b:>:c)

getTriples :: FL p wX wY -> [Sealed2 (p :> p :> p)]
getTriples NilFL = []
getTriples (_:>:NilFL) = []
getTriples (_:>:_:>:NilFL) = []
getTriples (a:>:b:>:c:>:d) = seal2 (a:>b:>c) : getTriples (b:>:c:>:d)

instance ( ArbitraryPrim prim
         , RepoModel (ModelOf prim)
         , model ~ ModelOf prim
         , ArbitraryState prim
         ) =>
         Arbitrary (Sealed (WithStartState model (TreeWithFlattenPos prim))) where
  arbitrary = do
    Sealed (WithStartState rm t) <- arbitrary
    case numPairs t of
      0 -> return $ Sealed $ WithStartState rm $ TWFP 0 NilTree
      num -> do
        n <- choose (0, num - 1)
        return $ Sealed $ WithStartState rm $ TWFP n t
