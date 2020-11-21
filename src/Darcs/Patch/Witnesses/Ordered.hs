-- Copyright (C) 2007 David Roundy
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

module Darcs.Patch.Witnesses.Ordered
    ( 
    -- * Directed Types
    -- $DirectedTypes
      (:>)(..)
    , FL(..)
    , RL(..)
    -- * Merge Types
    -- $MergeTypes
    , (:\/:)(..)
    , (:/\:)(..)
    , (:||:)(..)
    , Fork(..)
    -- * Functions for 'FL's and 'RL's
    , nullFL
    , nullRL
    , lengthFL
    , lengthRL
    , mapFL
    , mapRL
    , mapFL_FL
    , mapRL_RL
    , foldrFL
    , foldlRL
    , foldrwFL
    , foldlwRL
    , allFL
    , allRL
    , anyFL
    , anyRL
    , filterFL
    , filterRL
    , foldFL_M
    , foldRL_M
    , splitAtFL
    , splitAtRL
    , filterOutFLFL
    , filterOutRLRL
    , reverseFL
    , reverseRL
    , (+>+)
    , (+<+)
    , (+>>+)
    , (+<<+)
    , concatFL
    , concatRL
    , dropWhileFL
    , dropWhileRL
    -- * 'FL' only
    , bunchFL
    , spanFL
    , spanFL_M
    , zipWithFL
    , toFL
    , mapFL_FL_M
    , sequenceFL_
    , eqFL
    , eqFLUnsafe
    , initsFL
    -- * 'RL' only
    , isShorterThanRL
    , snocRLSealed
    , spanRL
    , breakRL
    , takeWhileRL
    , concatRLFL
    ) where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Sealed
    ( FlippedSeal(..)
    , flipSeal
    , Sealed(..)
    , FreeLeft
    , unFreeLeft
    , Sealed2(..)
    , seal
    )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )

-- * Directed Types

-- $DirectedTypes
-- Darcs patches have a notion of transforming between contexts. This
-- naturally leads us to container types that are \"directed\" and 
-- transform from one context to another.
-- 
-- For example, the swap of names of files x and y could be represented 
-- with the following sequence of patches:
-- 
-- @ Move x z ':>' Move y x ':>' Move z y @
-- 
-- or using forward lists, like
-- 
-- @ Move x z ':>:' Move y x ':>:' Move z y ':>:' NilFL @

-- | Directed Forward Pairs
data (a1 :> a2) wX wY = forall wZ . (a1 wX wZ) :> (a2 wZ wY)
infixr 1 :>

-- | Forward lists
data FL a wX wZ where
    (:>:) :: a wX wY -> FL a wY wZ -> FL a wX wZ
    NilFL :: FL a wX wX

-- | Reverse lists
data RL a wX wZ where
    (:<:) :: RL a wX wY -> a wY wZ -> RL a wX wZ
    NilRL :: RL a wX wX

instance Show2 a => Show (FL a wX wZ) where
   showsPrec _ NilFL = showString "NilFL"
   showsPrec d (x :>: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :>: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show1 (FL a wX)
instance Show2 a => Show2 (FL a)

instance Show2 a => Show (RL a wX wZ) where
   showsPrec _ NilRL = showString "NilRL"
   showsPrec d (xs :<: x) = showParen (d > prec) $ showsPrec (prec + 1) xs .
                            showString " :<: " . showsPrec2 (prec + 1) x
       where prec = 5

instance Show2 a => Show1 (RL a wX)

instance Show2 a => Show2 (RL a)

instance (Show2 a, Show2 b) => Show1 ((a :> b) wX)

-- * Merge Types

-- $MergeTypes
-- When we have two patches which commute and share the same pre-context we can
-- merge the patches. Whenever patches, or sequences of patches, share a
-- pre-context we say they are Forking Pairs (':\/:'). The same way, when
-- patches or sequences of patches, share a post-context we say they are
-- Joining Pairs (':/\:').
-- 
-- The following diagram shows the symmetry of merge types:
-- 
-- @          wZ
--         ':/\:'
--     a3 &#47;    &#92; a4  
--       &#47;      &#92;    
--      wX      wY       
--       &#92;      &#47;    
--     a1 &#92;    &#47; a2  
--         ':\/:'      
--          wZ
-- @

-- 
-- (non-haddock version)
--      wZ
--     :/\:
-- a3 /    \ a4
--   /      \
--  wX      wY
--   \      /
-- a1 \    / a2
--     :\/:
--      wZ
-- 

infix 1 :/\:, :\/:, :||:
-- | Forking Pairs (Implicit starting context)
data (a1 :\/: a2) wX wY = forall wZ . (a1 wZ wX) :\/: (a2 wZ wY)

-- | Joining Pairs
data (a3 :/\: a4) wX wY = forall wZ . (a3 wX wZ) :/\: (a4 wY wZ)

-- | Forking Pair (Explicit starting context)
-- 
-- @      wX     wY       
--       &#92;     &#47;    
--        &#92;   &#47;
--         &#92; &#47;     
--          wU
--          &#124;
--          &#124;
--          &#124;
--          wA
-- @

-- 
-- (non-haddock version)
-- 
--  wX      wY
--   \      /
--    \    /
--     \  /
--      wU
--      |
--      |
--      |
--      wA
-- 

data Fork common left right wA wX wY =
    forall wU. Fork (common wA wU) (left wU wX) (right wU wY)

-- | Parallel Pairs
data (a1 :||: a2) wX wY = (a1 wX wY) :||: (a2 wX wY)

instance (Show2 a, Show2 b) => Show ( (a :> b) wX wY ) where
    showsPrec d (x :> y) = showOp2 1 ":>" d x y

instance (Eq2 a, Eq2 b) => Eq2 (a :> b) where
    (a1 :> b1) =\/= (a2 :> b2) | IsEq <- a1 =\/= a2 = b1 =\/= b2
                               | otherwise = NotEq

instance (Eq2 a, Eq2 b) => Eq ((a :> b) wX wY) where
    (==) = unsafeCompare

instance (Show2 a, Show2 b) => Show2 (a :> b)

instance (Show2 a, Show2 b) => Show ( (a :\/: b) wX wY ) where
    showsPrec d (x :\/: y) = showOp2 9 ":\\/:" d x y

instance (Show2 a, Show2 b) => Show2 (a :\/: b)

instance (Show2 a, Show2 b) => Show ( (a :/\: b) wX wY ) where
    showsPrec d (x :/\: y) = showOp2 1 ":/\\:" d x y

instance (Show2 a, Show2 b) => Show2 ( (a :/\: b) )

-- * Functions

infixr 5 :>:, +>+
infixl 5 :<:, +<+

nullFL :: FL a wX wZ -> Bool
nullFL NilFL = True
nullFL _ = False

nullRL :: RL a wX wZ -> Bool
nullRL NilRL = True
nullRL _ = False

-- | @filterOutFLFL p xs@ deletes any @x@ in @xs@ for which @p x == IsEq@
--   (indicating that @x@ has no effect as far as we are concerned, and can be
--    safely removed from the chain)
filterOutFLFL :: (forall wX wY . p wX wY -> EqCheck wX wY) -> FL p wW wZ -> FL p wW wZ
filterOutFLFL _ NilFL = NilFL
filterOutFLFL f (x:>:xs) | IsEq <- f x = filterOutFLFL f xs
                         | otherwise = x :>: filterOutFLFL f xs

filterOutRLRL :: (forall wX wY . p wX wY -> EqCheck wX wY) -> RL p wW wZ -> RL p wW wZ
filterOutRLRL _ NilRL = NilRL
filterOutRLRL f (xs:<:x) | IsEq <- f x = filterOutRLRL f xs
                         | otherwise = filterOutRLRL f xs :<: x

filterRL :: (forall wX wY . p wX wY -> Bool) -> RL p wA wB ->  [Sealed2 p]
filterRL _ NilRL = []
filterRL f (xs :<: x) | f x = Sealed2 x : (filterRL f xs)
                      | otherwise = filterRL f xs

(+>+) :: FL a wX wY -> FL a wY wZ -> FL a wX wZ
NilFL +>+ ys = ys
(x:>:xs) +>+ ys = x :>: xs +>+ ys

(+<+) :: RL a wX wY -> RL a wY wZ -> RL a wX wZ
xs +<+ NilRL = xs
xs +<+ (ys:<:y) = xs +<+ ys :<: y

reverseFL :: FL a wX wZ -> RL a wX wZ
reverseFL xs = r NilRL xs
  where r :: RL a wL wM -> FL a wM wO -> RL a wL wO
        r ls NilFL = ls
        r ls (a:>:as) = r (ls:<:a) as

reverseRL :: RL a wX wZ -> FL a wX wZ
reverseRL xs = r NilFL xs
  where r :: FL a wM wO -> RL a wL wM -> FL a wL wO
        r ls NilRL = ls
        r ls (as:<:a) = r (a:>:ls) as

concatFL :: FL (FL a) wX wZ -> FL a wX wZ
concatFL NilFL = NilFL
concatFL (a:>:as) = a +>+ concatFL as

concatRL :: RL (RL a) wX wZ -> RL a wX wZ
concatRL NilRL = NilRL
concatRL (as:<:a) = concatRL as +<+ a

spanFL :: (forall wW wY . a wW wY -> Bool) -> FL a wX wZ -> (FL a :> FL a) wX wZ
spanFL f (x:>:xs) | f x = case spanFL f xs of
                            ys :> zs -> (x:>:ys) :> zs
spanFL _ xs = NilFL :> xs

spanFL_M :: forall a m wX wZ. Monad m =>
            (forall wW wY . a wW wY -> m Bool) -> FL a wX wZ
            -> m ((FL a :> FL a) wX wZ)
spanFL_M f (x:>:xs) =
    do
      continue <- f x
      if continue
       then do (ys :> zs) <- spanFL_M f xs
               return $ (x :>: ys) :> zs
       else return $ NilFL :> (x :>: xs)

spanFL_M _ (NilFL) = return $ NilFL :> NilFL

splitAtFL :: Int -> FL a wX wZ -> (FL a :> FL a) wX wZ
splitAtFL 0 xs = NilFL :> xs
splitAtFL _ NilFL = NilFL :> NilFL
splitAtFL n (x:>:xs) = case splitAtFL (n-1) xs of
                       (xs':>xs'') -> (x:>:xs' :> xs'')

splitAtRL :: Int -> RL a wX wZ -> (RL a :> RL a) wX wZ
splitAtRL 0 xs = xs :> NilRL
splitAtRL _ NilRL = NilRL :> NilRL
splitAtRL n (xs:<:x) = case splitAtRL (n-1) xs of
                       (xs'':>xs') -> (xs'':> xs':<:x)

-- 'bunchFL n' groups patches into batches of n, except that it always puts
-- the first patch in its own group, this being a recognition that the
-- first patch is often *very* large.

bunchFL :: Int -> FL a wX wY -> FL (FL a) wX wY
bunchFL _ NilFL = NilFL
bunchFL n (x:>:xs) = (x :>: NilFL) :>: bFL xs
    where bFL :: FL a wX wY -> FL (FL a) wX wY
          bFL NilFL = NilFL
          bFL bs = case splitAtFL n bs of
                   a :> b -> a :>: bFL b

-- | Monadic fold over an 'FL' associating to the left, sequencing
-- effects from left to right.
-- The order of arguments follows the standard 'foldM' from base.
foldFL_M :: Monad m
         => (forall wA wB. r wA -> p wA wB -> m (r wB))
         -> r wX -> FL p wX wY -> m (r wY)
foldFL_M _ r NilFL = return r
foldFL_M f r (x :>: xs) = f r x >>= \r' -> foldFL_M f r' xs

-- | Monadic fold over an 'FL' associating to the right, sequencing
-- effects from right to left.
-- Mostly useful for prepend-like operations with an effect where the
-- order of effects is not relevant.
foldRL_M :: Monad m
         => (forall wA wB. p wA wB -> r wB -> m (r wA))
         -> RL p wX wY -> r wY -> m (r wX)
foldRL_M _ NilRL r = return r
foldRL_M f (xs :<: x) r = f x r >>= foldRL_M f xs

allFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> Bool
allFL f xs = and $ mapFL f xs

anyFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> Bool
anyFL f xs = or $ mapFL f xs

allRL :: (forall wA wB . a wA wB -> Bool) -> RL a wX wY -> Bool
allRL f xs = and $ mapRL f xs

anyRL :: (forall wA wB . a wA wB -> Bool) -> RL a wX wY -> Bool
anyRL f xs = or $ mapRL f xs

-- | The "natural" fold over an 'FL' i.e. associating to the right.
-- Like 'Prelude.foldr' only with the more useful order of arguments.
foldrFL :: (forall wA wB . p wA wB -> r -> r) -> FL p wX wY -> r -> r
foldrFL _ NilFL r = r
foldrFL f (p:>:ps) r = f p (foldrFL f ps r)

-- | The "natural" fold over an RL i.e. associating to the left.
foldlRL :: (forall wA wB . r -> p wA wB -> r) -> r -> RL p wX wY -> r
foldlRL _ r NilRL = r
foldlRL f r (ps:<:p) = f (foldlRL f r ps) p

-- | Right associative fold for 'FL's that transforms a witnessed state
-- in the direction opposite to the 'FL'.
-- This is the "natural" fold for 'FL's i.e. the one which replaces the
-- ':>:' with the passed operator.
foldrwFL :: (forall wA wB . p wA wB -> r wB -> r wA) -> FL p wX wY -> r wY -> r wX
foldrwFL _ NilFL r = r
foldrwFL f (p:>:ps) r = f p (foldrwFL f ps r)

-- | The analog of 'foldrwFL' for 'RL's.
-- This is the "natural" fold for 'RL's i.e. the one which replaces the
-- ':<:' with the passed operator.
foldlwRL :: (forall wA wB . r wA -> p wA wB -> r wB) -> r wX -> RL p wX wY -> r wY
foldlwRL _ r NilRL = r
foldlwRL f r (ps:<:p) = f (foldlwRL f r ps) p

mapFL_FL :: (forall wW wY . a wW wY -> b wW wY) -> FL a wX wZ -> FL b wX wZ
mapFL_FL _ NilFL = NilFL
mapFL_FL f (a:>:as) = f a :>: mapFL_FL f as

mapFL_FL_M :: Monad m => (forall wW wY . a wW wY -> m (b wW wY)) -> FL a wX wZ -> m (FL b wX wZ)
mapFL_FL_M _ NilFL = return NilFL
mapFL_FL_M f (a:>:as) = do b <- f a
                           bs <- mapFL_FL_M f as
                           return (b:>:bs)

sequenceFL_ :: Monad m => (forall wW wZ . a wW wZ -> m b) -> FL a wX wY -> m ()
sequenceFL_ f = sequence_ . mapFL f

zipWithFL :: (forall wX wY . a -> p wX wY -> q wX wY)
          -> [a] -> FL p wW wZ -> FL q wW wZ
zipWithFL f (x:xs) (y :>: ys) = f x y :>: zipWithFL f xs ys
zipWithFL _ _ NilFL = NilFL
zipWithFL _ [] (_:>:_) = error "zipWithFL called with too short a list"

mapRL_RL :: (forall wW wY . a wW wY -> b wW wY) -> RL a wX wZ -> RL b wX wZ
mapRL_RL _ NilRL = NilRL
mapRL_RL f (as:<:a) = mapRL_RL f as :<: f a

{-# INLINABLE mapFL #-}
mapFL :: (forall wW wZ . a wW wZ -> b) -> FL a wX wY -> [b]
mapFL _ NilFL = []
mapFL f (a :>: b) = f a : mapFL f b

filterFL :: (forall wX wY . a wX wY -> Bool) -> FL a wW wZ -> [Sealed2 a]
filterFL _ NilFL = []
filterFL f (a :>: b) = if f a
                       then (Sealed2 a):(filterFL f b)
                       else filterFL f b

mapRL :: (forall wW wZ . a wW wZ -> b) -> RL a wX wY -> [b]
mapRL _ NilRL = []
mapRL f (as :<: a) = f a : mapRL f as

lengthFL :: FL a wX wZ -> Int
lengthFL xs = l xs 0
  where l :: FL a wX wZ -> Int -> Int
        l NilFL n = n
        l (_:>:as) n = l as $! n+1

lengthRL :: RL a wX wZ -> Int
lengthRL xs = l xs 0
  where l :: RL a wX wZ -> Int -> Int
        l NilRL n = n
        l (as:<:_) n = l as $! n+1

isShorterThanRL :: RL a wX wY -> Int -> Bool
isShorterThanRL _ n | n <= 0 = False
isShorterThanRL NilRL _ = True
isShorterThanRL (xs:<:_) n = isShorterThanRL xs (n-1)

snocRLSealed :: FlippedSeal (RL a) wY -> a wY wZ -> FlippedSeal (RL a) wZ
snocRLSealed (FlippedSeal as) a = flipSeal $ as :<: a

toFL :: [FreeLeft a] -> Sealed (FL a wX)
toFL [] = Sealed NilFL
toFL (x:xs) = case unFreeLeft x of Sealed y -> case toFL xs of Sealed ys -> Sealed (y :>: ys)

dropWhileFL :: (forall wX wY . a wX wY -> Bool) -> FL a wR wV -> FlippedSeal (FL a) wV
dropWhileFL _ NilFL       = flipSeal NilFL
dropWhileFL p xs@(x:>:xs')
          | p x       = dropWhileFL p xs'
          | otherwise = flipSeal xs

dropWhileRL :: (forall wX wY . a wX wY -> Bool) -> RL a wR wV -> Sealed (RL a wR)
dropWhileRL _ NilRL = seal NilRL
dropWhileRL p xs@(xs':<:x)
          | p x       = dropWhileRL p xs'
          | otherwise = seal xs

-- | Like 'takeWhile' only for 'RL's. This function is supposed to be lazy:
-- elements before the split point should not be touched.
takeWhileRL :: (forall wA wB . a wA wB -> Bool) -> RL a wX wY -> FlippedSeal (RL a) wY
takeWhileRL f xs = case spanRL f xs of _ :> r -> flipSeal r 

-- | Like 'span' only for 'RL's. This function is supposed to be lazy:
-- elements before the split point should not be touched.
spanRL :: (forall wA wB . p wA wB -> Bool) -> RL p wX wY -> (RL p :> RL p) wX wY
spanRL _ NilRL = NilRL :> NilRL
spanRL f left@(ps :<: p)
    | f p = case spanRL f ps of left' :> right -> left' :> right :<: p
    | otherwise = left :> NilRL

-- | Like 'break' only for 'RL's. This function is supposed to be lazy:
-- elements before the split point should not be touched.
breakRL :: (forall wA wB . p wA wB -> Bool) -> RL p wX wY -> (RL p :> RL p) wX wY
breakRL f = spanRL (not . f)

-- |Check that two 'FL's are equal element by element.
-- This differs from the 'Eq2' instance for 'FL' which
-- uses commutation.
eqFL :: Eq2 a => FL a wX wY -> FL a wX wZ -> EqCheck wY wZ
eqFL NilFL NilFL = IsEq
eqFL (x:>:xs) (y:>:ys) | IsEq <- x =\/= y, IsEq <- eqFL xs ys = IsEq
eqFL _ _ = NotEq

eqFLUnsafe :: Eq2 a => FL a wX wY -> FL a wZ wW -> Bool
eqFLUnsafe NilFL NilFL = True
eqFLUnsafe (x:>:xs) (y:>:ys) = unsafeCompare x y && eqFLUnsafe xs ys
eqFLUnsafe _ _ = False

infixr 5 +>>+
infixl 5 +<<+

-- | Prepend an 'RL' to an 'FL'. This traverses only the left hand side.
(+>>+) :: RL p wX wY -> FL p wY wZ -> FL p wX wZ
NilRL +>>+ ys = ys
(xs:<:x) +>>+ ys = xs +>>+ (x :>: ys)

-- | Append an 'FL' to an 'RL'. This traverses only the right hand side.
(+<<+) :: RL p wX wY -> FL p wY wZ -> RL p wX wZ
xs +<<+ NilFL = xs
xs +<<+ (y:>:ys) = (xs:<:y) +<<+ ys

initsFL :: FL p wX wY -> [Sealed ((p :> FL p) wX)]
initsFL NilFL = []
initsFL (x :>: xs) =
    Sealed (x :> NilFL) :
    map (\(Sealed (y :> xs')) -> Sealed (x :> y :>: xs')) (initsFL xs)

concatRLFL :: RL (FL p) wX wY -> RL p wX wY
concatRLFL NilRL = NilRL
concatRLFL (ps :<: p) = concatRLFL ps +<<+ p
