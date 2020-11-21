-- Copyright (C) 2002-2003 David Roundy
-- Copyright (C) 2009 Ganesh Sittampalam
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Darcs.Patch.Permutations ( removeFL, removeRL, removeCommon,
                                  commuteWhatWeCanFL, commuteWhatWeCanRL,
                                  genCommuteWhatWeCanRL, genCommuteWhatWeCanFL,
                                  partitionFL, partitionRL, partitionFL',
                                  simpleHeadPermutationsFL, headPermutationsRL,
                                  headPermutationsFL, permutationsRL,
                                  removeSubsequenceFL, removeSubsequenceRL,
                                  partitionConflictingFL
                                ) where

import Darcs.Prelude

import Data.Maybe ( mapMaybe )
import Darcs.Patch.Commute ( Commute, commute, commuteFL, commuteRL )
import Darcs.Patch.Merge ( CleanMerge(..), cleanMergeFL )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..), (:\/:)(..), (:/\:)(..)
    , (+<+), (+>+)
    , lengthFL, lengthRL
    , reverseFL, reverseRL
    )

-- | Split an 'FL' according to a predicate, using commutation as necessary,
-- into those that satisfy the predicate and can be commuted to the left, and
-- those that do not satisfy it and can be commuted to the right. Whatever
-- remains stays in the middle.
--
-- Note that the predicate @p@ should be invariant under commutation:
-- if @commute(x:>y)==Just(y':>x')@ then @p x == p x' && p y == p y'@.
partitionFL :: Commute p
            => (forall wU wV . p wU wV -> Bool)  -- ^predicate; if true we would like the patch in the "left" list
            -> FL p wX wY                        -- ^input 'FL'
            -> (FL p :> FL p :> FL p) wX wY      -- ^"left", "middle" and "right"
partitionFL keepleft ps =
    case partitionFL' keepleft NilRL NilRL ps of
        left :> middle :> right -> left :> reverseRL middle :> reverseRL right

-- optimise by using an accumulating parameter to track all the "left"
-- patches that we've found so far; also do not reverse the result lists
partitionFL' :: Commute p
             => (forall wU wV . p wU wV -> Bool)
             -> RL p wA wB  -- the "middle" patches found so far
             -> RL p wB wC  -- the "right" patches found so far
             -> FL p wC wD
             -> (FL p :> RL p :> RL p) wA wD
partitionFL' _ middle right NilFL = NilFL :> middle :> right
partitionFL' keepleft middle right (p :>: ps)
    | keepleft p = case commuteWhatWeCanRL (right :> p) of
        (NilRL :> p' :> right') -> case commuteRL (middle :> p') of
            Just (p'' :> middle') -> case partitionFL' keepleft middle' right' ps of
                (a :> b :> c) -> p'' :>: a :> b :> c
            Nothing -> partitionFL' keepleft (middle :<: p') right' ps
        (tomiddle :> p' :> right') ->
            partitionFL' keepleft (middle +<+ tomiddle :<: p') right' ps
    | otherwise = partitionFL' keepleft middle (right :<: p) ps

-- | Split an 'RL' according to a predicate, using commutation as necessary,
-- into those that satisfy the predicate and can be commuted to the right, and
-- those that don't, i.e. either do not satisfy the predicate or cannot be
-- commuted to the right.
--
-- Note that the predicate @p@ should be invariant under commutation:
-- if @commute(x:>y)==Just(y':>x')@ then @p x == p x' && p y == p y'@.
partitionRL :: forall p wX wY. Commute p
            => (forall wU wV . p wU wV -> Bool) -- ^predicate; if true we would like the patch in the "right" list
            -> RL p wX wY                       -- ^input 'RL'
            -> (RL p :> RL p) wX wY             -- ^"left" and "right" results
partitionRL keepright = go . (:> NilFL)
  where
    go :: (RL p :> FL p) wA wB -> (RL p :> RL p) wA wB
    go (NilRL :> qs) = (reverseFL qs :> NilRL)
    go (ps :<: p :> qs)
      | keepright p
      , Just (qs' :> p') <- commuteFL (p :> qs) =
          case go (ps :> qs') of
            a :> b -> a :> b :<: p'
      | otherwise = go (ps :> p :>: qs)

commuteWhatWeCanFL :: Commute p => (p :> FL p) wX wY -> (FL p :> p :> FL p) wX wY
commuteWhatWeCanFL = genCommuteWhatWeCanFL commute

genCommuteWhatWeCanFL :: Commute q =>
                         (forall wA wB . ((p:>q) wA wB -> Maybe ((q:>p)wA wB)))
                          -> (p :> FL q) wX wY -> (FL q :> p :> FL q) wX wY
genCommuteWhatWeCanFL com (p :> x :>: xs) =
    case com (p :> x) of
    Nothing -> case commuteWhatWeCanFL (x :> xs) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanFL com (p :> xs1) of
                              xs1' :> p' :> xs2' -> xs1' :> p' :> xs2' +>+ x' :>: xs2
    Just (x' :> p') -> case genCommuteWhatWeCanFL com (p' :> xs) of
                       a :> p'' :> c -> x' :>: a :> p'' :> c
genCommuteWhatWeCanFL _ (y :> NilFL) = NilFL :> y :> NilFL

commuteWhatWeCanRL :: Commute p => (RL p :> p) wX wY -> (RL p :> p :> RL p) wX wY
commuteWhatWeCanRL = genCommuteWhatWeCanRL commute

genCommuteWhatWeCanRL :: Commute p =>
                         (forall wA wB . ((p :> q) wA wB -> Maybe ((q :> p) wA wB)))
                         -> (RL p :> q) wX wY -> (RL p :> q :> RL p) wX wY
genCommuteWhatWeCanRL com (xs :<: x :> p) =
    case com (x :> p) of
    Nothing -> case commuteWhatWeCanRL (xs :> x) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanRL com (xs2 :> p) of
                              xs1' :> p' :> xs2' -> xs1 :<: x' +<+ xs1' :> p' :> xs2'
    Just (p' :> x') -> case genCommuteWhatWeCanRL com (xs :> p') of
                       xs1 :> p'' :> xs2 -> xs1 :> p'' :> xs2 :<: x'
genCommuteWhatWeCanRL _ (NilRL :> y) = NilRL :> y :> NilRL


removeCommon :: (Eq2 p, Commute p) => (FL p :\/: FL p) wX wY -> (FL p :\/: FL p) wX wY
removeCommon (xs :\/: NilFL) = xs :\/: NilFL
removeCommon (NilFL :\/: xs) = NilFL :\/: xs
removeCommon (xs :\/: ys) = rc xs (headPermutationsFL ys)
    where rc :: (Eq2 p, Commute p) => FL p wX wY -> [(p:>FL p) wX wZ] -> (FL p :\/: FL p) wY wZ
          rc nms ((n:>ns):_) | Just ms <- removeFL n nms = removeCommon (ms :\/: ns)
          rc ms [n:>ns] = ms :\/: n:>:ns
          rc ms (_:nss) = rc ms nss
          rc _ [] = error "impossible case" -- because we already checked for NilFL case

-- | 'removeFL' @x xs@ removes @x@ from @xs@ if @x@ can be commuted to its head.
--   Otherwise it returns 'Nothing'
removeFL :: (Eq2 p, Commute p) => p wX wY -> FL p wX wZ -> Maybe (FL p wY wZ)
removeFL x xs = r x $ headPermutationsFL xs
    where r :: (Eq2 p, Commute p) => p wX wY -> [(p:>FL p) wX wZ] -> Maybe (FL p wY wZ)
          r _ [] = Nothing
          r z ((z':>zs):zss) | IsEq <- z =\/= z' = Just zs
                             | otherwise = r z zss

-- | 'removeRL' is like 'removeFL' except with 'RL'
removeRL :: (Eq2 p, Commute p) => p wY wZ -> RL p wX wZ -> Maybe (RL p wX wY)
removeRL x xs = r x $ headPermutationsRL xs
    where r :: (Eq2 p, Commute p) => p wY wZ -> [RL p wX wZ] -> Maybe (RL p wX wY)
          r z ((zs:<:z'):zss) | IsEq <- z =/\= z' = Just zs
                              | otherwise = r z zss
          r _ _ = Nothing

-- | 'removeSubsequenceFL' @ab abc@ returns @Just c'@ where all the patches in
--   @ab@ have been commuted out of it, if possible.  If this is not possible
--   for any reason (the set of patches @ab@ is not actually a subset of @abc@,
--   or they can't be commuted out) we return 'Nothing'.
removeSubsequenceFL :: (Eq2 p, Commute p) => FL p wA wB
                     -> FL p wA wC -> Maybe (FL p wB wC)
removeSubsequenceFL a b | lengthFL a > lengthFL b = Nothing
                         | otherwise = rsFL a b
    where rsFL :: (Eq2 p, Commute p) => FL p wA wB -> FL p wA wC -> Maybe (FL p wB wC)
          rsFL NilFL ys = Just ys
          rsFL (x:>:xs) yys = removeFL x yys >>= removeSubsequenceFL xs

-- | 'removeSubsequenceRL' is like @removeSubsequenceFL@ except that it works
--   on 'RL'
removeSubsequenceRL :: (Eq2 p, Commute p) => RL p wAb wAbc
                     -> RL p wA wAbc -> Maybe (RL p wA wAb)
removeSubsequenceRL a b | lengthRL a > lengthRL b = Nothing
                         | otherwise = rsRL a b
    where rsRL :: (Eq2 p, Commute p) => RL p wAb wAbc -> RL p wA wAbc -> Maybe (RL p wA wAb)
          rsRL NilRL ys = Just ys
          rsRL (xs:<:x) yys = removeRL x yys >>= removeSubsequenceRL xs

-- | This is a minor variant of 'headPermutationsFL' with each permutation
--   is simply returned as a 'FL'
simpleHeadPermutationsFL :: Commute p => FL p wX wY -> [FL p wX wY]
simpleHeadPermutationsFL ps = map (\ (x:>xs) -> x:>:xs) $ headPermutationsFL ps

-- | 'headPermutationsFL' @p:>:ps@ returns all the permutations of the list
--   in which one element of @ps@ is commuted past @p@
--
--   Suppose we have a sequence of patches
--
--   >  X h a y s-t-c k
--
--   Suppose furthermore that the patch @c@ depends on @t@, which in turn
--   depends on @s@.  This function will return
--
--   > X :> h a y s t c k
--   > h :> X a y s t c k
--   > a :> X h y s t c k
--   > y :> X h a s t c k
--   > s :> X h a y t c k
--   > k :> X h a y s t c
headPermutationsFL :: Commute p => FL p wX wY -> [(p :> FL p) wX wY]
headPermutationsFL NilFL = []
headPermutationsFL (p:>:ps) =
    (p:>ps) : mapMaybe (swapfirstFL.(p:>)) (headPermutationsFL ps)
        where swapfirstFL (p1:>p2:>xs) = do p2':>p1' <- commute (p1:>p2)
                                            Just $ p2':>p1':>:xs

-- | 'headPermutationsRL' is like 'headPermutationsFL', except that we
--   operate on an 'RL' (in other words, we are pushing things to the end of a
--   patch sequence instead of to the beginning).
headPermutationsRL :: Commute p => RL p wX wY -> [RL p wX wY]
headPermutationsRL NilRL = []
headPermutationsRL (ps:<:p) =
    (ps:<:p) : mapMaybe (swapfirstRL.(:<:p)) (headPermutationsRL ps)
        where swapfirstRL (xs:<:p2:<:p1) = do p1':>p2' <- commute (p2:>p1)
                                              Just $ xs:<:p1':<:p2'
              swapfirstRL _ = Nothing

-- | All permutations of an 'RL'.
permutationsRL :: Commute p => RL p wX wY -> [RL p wX wY]
permutationsRL ps =
  [qs' :<: q | qs :<: q <- headPermutationsRL ps, qs' <- permutationsRL qs]

instance (Eq2 p, Commute p) => Eq2 (FL p) where
    a =\/= b | lengthFL a /= lengthFL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: FL p wX wY -> FL p wX wZ -> EqCheck wY wZ
                   cmpSameLength (x:>:xs) xys | Just ys <- removeFL x xys = cmpSameLength xs ys
                   cmpSameLength NilFL NilFL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =/\= ys = reverseFL xs =/\= reverseFL ys

instance (Eq2 p, Commute p) => Eq2 (RL p) where
    unsafeCompare = error "Buggy use of unsafeCompare on RL"
    a =/\= b | lengthRL a /= lengthRL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: RL p wX wY -> RL p wW wY -> EqCheck wX wW
                   cmpSameLength (xs:<:x) xys | Just ys <- removeRL x xys = cmpSameLength xs ys
                   cmpSameLength NilRL NilRL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =\/= ys = reverseRL xs =\/= reverseRL ys

-- | Partition a list into the patches that merge cleanly with the given
-- patch and those that don't (including dependencies)
partitionConflictingFL :: (Commute p, CleanMerge p)
                       => FL p wX wY -> FL p wX wZ -> (FL p :> FL p) wX wY
partitionConflictingFL NilFL _ = NilFL :> NilFL
partitionConflictingFL (x :>: xs) ys =
  case cleanMergeFL (x :\/: ys) of
    Nothing ->
      case commuteWhatWeCanFL (x :> xs) of
        xs_ok :> x' :> xs_deps ->
          case partitionConflictingFL xs_ok ys of
            xs_clean :> xs_conflicts ->
              xs_clean :> (xs_conflicts +>+ (x' :>: xs_deps))
    Just (ys' :/\: _) ->
      case partitionConflictingFL xs ys' of
        xs_clean :> xs_conflicts -> (x :>: xs_clean) :> xs_conflicts
