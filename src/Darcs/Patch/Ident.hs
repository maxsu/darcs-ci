module Darcs.Patch.Ident
    ( Ident(..)
    , SignedIdent
    , PatchId
    , SignedId(..)
    , StorableId(..)
    , IdEq2(..)
    , merge2FL
    , fastRemoveFL
    , fastRemoveRL
    , fastRemoveSubsequenceRL
    , findCommonFL
    , commuteToPrefix
    , commuteToPostfix
    , commuteWhatWeCanToPostfix
    -- * Properties
    , prop_identInvariantUnderCommute
    , prop_sameIdentityImpliesCommutable
    , prop_equalImpliesSameIdentity
    ) where

import qualified Data.Set as S

import Darcs.Prelude

import Darcs.Patch.Commute ( Commute, commute, commuteFL, commuteRL )
import Darcs.Patch.Merge ( Merge, mergeFL )
import Darcs.Patch.Permutations ( partitionFL', commuteWhatWeCanFL )
import Darcs.Patch.Show ( ShowPatchFor )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..), isIsEq )
import Darcs.Patch.Witnesses.Ordered
    ( (:/\:)(..)
    , (:>)(..)
    , (:\/:)(..)
    , FL(..)
    , RL(..)
    , Fork(..)
    , (+<<+)
    , (+>>+)
    , mapFL
    , mapRL
    , reverseRL
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd, unsafeCoercePStart )

import Darcs.Util.Parser ( Parser )
import Darcs.Util.Printer ( Doc )

type family PatchId (p :: * -> * -> *)

{- | Class of patches that have an identity.

It generalizes named prim patches a la camp (see Darcs.Patch.Prim.Named) and
Named patches i.e. those with a PatchInfo.

Patch identity should be invariant under commutation: if there is also an
@instance 'Commute' p@, then

prop> commute (p :> q) == Just (q' :> p') => ident p == ident p' && ident q == ident q'

The converse should also be true: patches with the same identity can be
commuted (back) to the same context and then compare equal. Assuming

@
  p :: p wX wY, (ps :> q) :: (RL p :> p) wX wZ
@

then

prop> ident p == ident q => commuteRL (ps :> q) == Just (p :> _)

As a special case we get that parallel patches with the same identity are
equal: if @p :: p wX wY, q :: p wX wZ@, then

prop> ident p == ident q => p =\/= q == IsEq

In general, comparing patches via their identity is coarser than
(structural) equality, so we only have

prop> unsafeCompare p q => (ident p == ident q)
-}
class Ord (PatchId p) => Ident p where
  ident :: p wX wY -> PatchId p

{- | Signed identities.

Like for class 'Invert', we require that 'invertId' is self-inverse:

prop> invertId . invertId = id

We also require that inverting changes the sign:

prop> positiveId . invertId = not . positiveId

Side remark: in mathematical terms, these properties can be expressed by
stating that 'invertId' is an involution and that 'positiveId' is a
"homomorphism of sets with an involution" (there is no official term for
this) from @a@ to the simplest non-trivial set with involution, namely
'Bool' with the involution 'not'.
-}
class Ord a => SignedId a where
  positiveId :: a -> Bool
  invertId :: a -> a

{- | Constraint for patches that have an identity that is signed,
     i.e. can be positive (uninverted) or negative (inverted).

Provided that an instance 'Invert' exists, inverting a patch
inverts its identity:

prop> ident (invert p) = invertId (ident p)

-}
type SignedIdent p = (Ident p, SignedId (PatchId p))


{- | Storable identities.

The methods here can be used to help implement ReadPatch and ShowPatch
for a patch type containing the identity.

As with all Read/Show pairs, We expect that the output of
@showId ForStorage a@ can be parsed by 'readId' to produce @a@.
-}
class StorableId a where
  readId :: Parser a
  showId :: ShowPatchFor -> a -> Doc

-- | Faster equality tests for patches with an identity.
class IdEq2 p where
  (=\^/=) :: p wA wB -> p wA wC -> EqCheck wB wC
  (=/^\=) :: p wA wC -> p wB wC -> EqCheck wA wB
  default (=\^/=) :: Ident p => p wA wB -> p wA wC -> EqCheck wB wC
  p =\^/= q = if ident p == ident q then unsafeCoercePEnd IsEq else NotEq
  default (=/^\=) :: Ident p => p wA wC -> p wB wC -> EqCheck wA wB
  p =/^\= q = if ident p == ident q then unsafeCoercePStart IsEq else NotEq

-- | The 'Commute' requirement here is not technically needed but makes
-- sense logically.
instance (Commute p, Ident p) => IdEq2 (FL p) where
  ps =\^/= qs
    | S.fromList (mapFL ident ps) == S.fromList (mapFL ident qs) = unsafeCoercePEnd IsEq
    | otherwise = NotEq
  ps =/^\= qs
    | S.fromList (mapFL ident ps) == S.fromList (mapFL ident qs) = unsafeCoercePStart IsEq
    | otherwise = NotEq

-- | This function is similar to 'merge', but with one important
-- difference: 'merge' works on patches for which there is not necessarily a
-- concept of identity (e.g. primitive patches, conflictors, etc). Thus it does
-- not even try to recognize patches that are common to both sequences. Instead
-- these are passed on to the Merge instance for single patches. This instance
-- may handle duplicate patches by creating special patches (Duplicate,
-- Conflictor).
-- 
-- We do not want this to happen for named patches, or in general for patches
-- with an identity. Instead, we want to
-- /discard/ one of the two duplicates, retaining only one copy. This is done
-- by the fastRemoveFL calls below. We call mergeFL only after we have ensured
-- that the head of the left hand side does not occur in the right hand side.
merge2FL :: (Commute p, Merge p, Ident p)
         => FL p wX wY
         -> FL p wX wZ
         -> (FL p :/\: FL p) wY wZ
merge2FL xs NilFL = NilFL :/\: xs
merge2FL NilFL ys = ys :/\: NilFL
merge2FL xs (y :>: ys)
  | Just xs' <- fastRemoveFL y xs = merge2FL xs' ys
merge2FL (x :>: xs) ys
  | Just ys' <- fastRemoveFL x ys = merge2FL xs ys'
  | otherwise =
    case mergeFL (x :\/: ys) of
      ys' :/\: x' ->
        case merge2FL xs ys' of
          ys'' :/\: xs' -> ys'' :/\: (x' :>: xs')

{-# INLINABLE fastRemoveFL #-}
-- | Remove a patch from an FL of patches with an identity. The result is
-- 'Just' whenever the patch has been found and removed and 'Nothing'
-- otherwise. If the patch is not found at the head of the sequence we must
-- first commute it to the head before we can remove it.
-- 
-- We assume that this commute always succeeds. This is justified because
-- patches are created with a (universally) unique identity, implying that if
-- two patches have the same identity, then they have originally been the same
-- patch; thus being at a different position must be due to commutation,
-- meaning we can commute it back.
fastRemoveFL :: forall p wX wY wZ. (Commute p, Ident p)
             => p wX wY
             -> FL p wX wZ
             -> Maybe (FL p wY wZ)
fastRemoveFL a bs
  | i `notElem` mapFL ident bs = Nothing
  | otherwise = do
      _ :> bs' <- pullout NilRL bs
      Just (unsafeCoercePStart bs')
  where
    i = ident a
    pullout :: RL p wA wB -> FL p wB wC -> Maybe ((p :> FL p) wA wC)
    pullout _ NilFL = Nothing
    pullout acc (x :>: xs)
      | ident x == i = do
          x' :> acc' <- commuteRL (acc :> x)
          Just (x' :> acc' +>>+ xs)
      | otherwise = pullout (acc :<: x) xs

-- | Same as 'fastRemoveFL' only for 'RL'.
fastRemoveRL :: forall p wX wY wZ. (Commute p, Ident p)
             => p wY wZ
             -> RL p wX wZ
             -> Maybe (RL p wX wY)
fastRemoveRL a bs
  | i `notElem` mapRL ident bs = Nothing
  | otherwise = do
      bs' :> _ <- pullout bs NilFL
      Just (unsafeCoercePEnd bs')
  where
    i = ident a
    pullout :: RL p wA wB -> FL p wB wC -> Maybe ((RL p :> p) wA wC)
    pullout NilRL _ = Nothing
    pullout (xs :<: x) acc
      | ident x == i = do
          acc' :> x' <- commuteFL (x :> acc)
          Just (xs +<<+ acc' :> x')
      | otherwise = pullout xs (x :>: acc)

fastRemoveSubsequenceRL :: (Commute p, Ident p)
                        => RL p wY wZ
                        -> RL p wX wZ
                        -> Maybe (RL p wX wY)
fastRemoveSubsequenceRL NilRL ys = Just ys
fastRemoveSubsequenceRL (xs :<: x) ys =
  fastRemoveRL x ys >>= fastRemoveSubsequenceRL xs

-- | Find the common and uncommon parts of two lists that start in a common
-- context, using patch identity for comparison. Of the common patches, only
-- one is retained, the other is discarded, similar to 'merge2FL'.
findCommonFL :: (Commute p, Ident p)
             => FL p wX wY
             -> FL p wX wZ
             -> Fork (FL p) (FL p) (FL p) wX wY wZ
findCommonFL xs ys =
  case commuteToPrefix commonIds xs of
    Nothing -> error "failed to commute common patches (lhs)"
    Just (cxs :> xs') ->
      case commuteToPrefix commonIds ys of
        Nothing -> error "failed to commute common patches (rhs)"
        Just (cys :> ys') ->
          case cxs =\^/= cys of
            NotEq -> error "common patches aren't equal"
            IsEq -> Fork cxs (reverseRL xs') (reverseRL ys')
  where
    commonIds =
      S.fromList (mapFL ident xs) `S.intersection` S.fromList (mapFL ident ys)

-- | Try to commute patches matching any of the 'PatchId's in the set to the
-- head of an 'FL', i.e. backwards in history. It is not required that all the
-- 'PatchId's are found in the sequence, but if they do then the traversal
-- terminates as soon as the set is exhausted.
commuteToPrefix :: (Commute p, Ident p)
                => S.Set (PatchId p) -> FL p wX wY -> Maybe ((FL p :> RL p) wX wY)
commuteToPrefix is ps
  | prefix :> NilRL :> rest <-
      partitionFL' ((`S.member` is) . ident) NilRL NilRL ps = Just (prefix :> rest)
  | otherwise = Nothing

-- | Try to commute patches matching any of the 'PatchId's in the set to the
-- head of an 'RL', i.e. forwards in history. It is not required that all the
-- 'PatchId's are found in the sequence, but if they do then the traversal
-- terminates as soon as the set is exhausted.
commuteToPostfix :: forall p wX wY. (Commute p, Ident p)
                 => S.Set (PatchId p) -> RL p wX wY -> Maybe ((FL p :> RL p) wX wY)
commuteToPostfix ids patches = push ids (patches :> NilFL)
  where
    push :: S.Set (PatchId p) -> (RL p :> FL p) wA wB -> Maybe ((FL p :> RL p) wA wB)
    push _ (NilRL :> left) = return (left :> NilRL) -- input RL is ehausted
    push is (ps :> left)
      | S.null is = return (ps +>>+ left :> NilRL) -- set of IDs is exhausted
    push is (ps :<: p :> left)
      | let i = ident p
      , i `S.member` is = do
          left' :> p' <- commuteFL (p :> left)
          left'' :> right <- push (S.delete i is) (ps :> left')
          return (left'' :> right :<: p')
      | otherwise = push is (ps :> p :>: left)

-- | Like 'commuteToPostfix' but drag dependencies with us.
commuteWhatWeCanToPostfix :: forall p wX wY. (Commute p, Ident p)
                          => S.Set (PatchId p) -> RL p wX wY -> (FL p :> RL p) wX wY
commuteWhatWeCanToPostfix ids patches = push ids (patches :> NilFL)
  where
    push :: S.Set (PatchId p) -> (RL p :> FL p) wA wB -> (FL p :> RL p) wA wB
    push _ (NilRL :> left) = left :> NilRL -- input RL is ehausted
    push is (ps :> left)
      | S.null is = ps +>>+ left :> NilRL -- set of IDs is exhausted
    push is (ps :<: p :> left)
      | let i = ident p
      , i `S.member` is =
          case commuteWhatWeCanFL (p :> left) of
            left' :> p' :> deps ->
              case push (S.delete i is) (ps :> left') of
                left'' :> right -> left'' :> (right :<: p' +<<+ deps)
      | otherwise = push is (ps :> p :>: left)

prop_identInvariantUnderCommute :: (Commute p, Ident p)
                                => (p :> p) wX wY -> Maybe Bool
prop_identInvariantUnderCommute (p :> q) =
  case commute (p :> q) of
    Just (q' :> p') -> Just $ ident p == ident p' && ident q == ident q'
    Nothing -> Nothing

prop_sameIdentityImpliesCommutable :: (Commute p, Eq2 p, Ident p)
                                   => (p :\/: (RL p :> p)) wX wY -> Maybe Bool
prop_sameIdentityImpliesCommutable (p :\/: (ps :> q))
  | ident p == ident q =
      case commuteRL (ps :> q) of
        Just (p' :> _) -> Just $ isIsEq (p =\/= p')
        Nothing -> Just False
  | otherwise = Nothing

prop_equalImpliesSameIdentity :: (Eq2 p, Ident p)
                              => (p :\/: p) wX wY -> Maybe Bool
prop_equalImpliesSameIdentity (p :\/: q)
  | IsEq <- p =\/= q = Just $ ident p == ident q
  | otherwise = Nothing
