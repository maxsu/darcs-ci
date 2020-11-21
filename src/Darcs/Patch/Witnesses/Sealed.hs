-- Copyright (C) 2007 David Roundy, 2009 Ganesh Sittampalam
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

{-# OPTIONS_HADDOCK ignore-exports #-}

module Darcs.Patch.Witnesses.Sealed
    ( Sealed(..)
    , seal
    , unseal
    , mapSeal
    , Sealed2(..)
    , seal2
    , unseal2
    , mapSeal2
    , FlippedSeal(..)
    , flipSeal
    , unsealFlipped
    , mapFlipped
    , Gap(..)
    , FreeLeft
    , unFreeLeft
    , FreeRight
    , unFreeRight
    ) where

import Darcs.Prelude

import Data.Functor.Compose ( Compose(..) )

import Darcs.Patch.Witnesses.Eq ( Eq2, EqCheck(..) )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Witnesses.Eq ( (=\/=) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP1, unsafeCoerceP )

-- |A 'Sealed' type is a way of hide an existentially quantified type parameter,
-- in this case wX, inside the type. Note that the only thing we can currently
-- recover about the existentially quantified type wX is that it exists.
data Sealed a where
    Sealed :: a wX -> Sealed a

seal :: a wX -> Sealed a
seal = Sealed

instance Eq2 a => Eq (Sealed (a wX)) where
    Sealed x == Sealed y | IsEq <- x =\/= y = True
                         | otherwise = False

-- |The same as 'Sealed' but for two parameters (wX and wY).
data Sealed2 a where
    Sealed2 :: !(a wX wY) -> Sealed2 a

seal2 :: a wX wY -> Sealed2 a
seal2 = Sealed2

data FlippedSeal a wY where
    FlippedSeal :: !(a wX wY) -> FlippedSeal a wY

flipSeal :: a wX wY -> FlippedSeal a wY
flipSeal = FlippedSeal

unsafeUnseal :: Sealed a -> a wX
unsafeUnseal (Sealed a) = unsafeCoerceP1 a

unsafeUnseal2 :: Sealed2 a -> a wX wY
unsafeUnseal2 (Sealed2 a) = unsafeCoerceP a

unseal :: (forall wX . a wX -> b) -> Sealed a -> b
unseal f x = f (unsafeUnseal x)

-- laziness property:
-- unseal (const True) undefined == True
--
-- Pattern-matching on Sealed is currently strict in GHC because it's an existential:
-- https://gitlab.haskell.org/ghc/ghc/issues/17130
-- Implementing unseal via unsafeUnseal (with the unsafeCoerceP1 underneath) works
-- around that, and we rely on that occasionally for performance, e.g. when reading
-- the history of a repository.
--
-- TODO: this is quite obscure and makes it hard to know whether we really need
-- laziness in a certain place or are just getting it incidentally because someone
-- chose to use unseal rather than pattern-matching. We should introduce an explicit
-- "Make this Sealed lazy" combinator (also using unsafeCoerceP1 in the implementation)
-- and then make the seal implementation itself be strict.
--
-- The combinator would work by making a value with a fresh Sealed constructor, so even
-- though the subsequent pattern-match/unseal on that would itself be strict, it would
-- only force as far as the newly introduced Sealed.
--
-- All this applies to Sealed2 too, and FlippedSeal if we ever need a lazy one (but
-- the implementation of unsealFlipped has been strict for a long time without causing
-- trouble).

mapSeal :: (forall wX . a wX -> b wX) -> Sealed a -> Sealed b
mapSeal f = unseal (seal . f)

mapFlipped :: (forall wX . a wX wY -> b wX wZ) -> FlippedSeal a wY -> FlippedSeal b wZ
mapFlipped f (FlippedSeal x) = FlippedSeal (f x)

unseal2 :: (forall wX wY . a wX wY -> b) -> Sealed2 a -> b
unseal2 f a = f (unsafeUnseal2 a)

mapSeal2 :: (forall wX wY . a wX wY -> b wX wY) -> Sealed2 a -> Sealed2 b
mapSeal2 f = unseal2 (seal2 . f)

unsealFlipped :: (forall wX wY . a wX wY -> b) -> FlippedSeal a wZ -> b
unsealFlipped f (FlippedSeal a) = f a

instance Show1 a => Show (Sealed a) where
    showsPrec d (Sealed x) = showParen (d > appPrec) $ showString "Sealed " . showsPrec1 (appPrec + 1) x
instance Show2 a => Show (Sealed2 a) where
    showsPrec d (Sealed2 x) = showParen (d > appPrec) $ showString "Sealed2 " . showsPrec2 (appPrec + 1) x

-- |'Poly' is similar to 'Sealed', but the type argument is
-- universally quantified instead of being existentially quantified.
newtype Poly a = Poly { unPoly :: forall wX . a wX }

-- |'FreeLeft' p is @ \forall x . \exists y . p x y @
-- In other words the caller is free to specify the left witness,
-- and then the right witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that @ y @ is dependent on the @ x @ that is supplied.
-- This is why 'Stepped' is needed, rather than writing the more obvious
-- 'Sealed' ('Poly' p) which would notionally have the same quantification
-- of the type witnesses.
newtype FreeLeft p = FLInternal (Poly (Compose Sealed p))

-- |'FreeRight' p is @ \forall y . \exists x . p x y @
-- In other words the caller is free to specify the right witness,
-- and then the left witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that @ x @ is dependent on the @ y @ that is supplied.
newtype FreeRight p = FRInternal (Poly (FlippedSeal p))

-- |Unwrap a 'FreeLeft' value
unFreeLeft :: FreeLeft p -> Sealed (p wX)
unFreeLeft (FLInternal x) = getCompose (unPoly x)

-- |Unwrap a 'FreeRight' value
unFreeRight :: FreeRight p -> FlippedSeal p wX
unFreeRight (FRInternal x) = unPoly x

-- |'Gap' abstracts over 'FreeLeft' and 'FreeRight' for code constructing these values
class Gap w where
  -- |An empty 'Gap', e.g. 'NilFL' or 'NilRL'
  emptyGap :: (forall wX . p wX wX) -> w p
  -- |A 'Gap' constructed from a completely polymorphic value, for example the constructors
  -- for primitive patches
  freeGap :: (forall wX wY . p wX wY) -> w p

  -- |Compose two 'Gap' values together in series, e.g. 'joinGap (+>+)' or 'joinGap (:>:)'
  joinGap :: (forall wX wY wZ . p wX wY -> q wY wZ -> r wX wZ) -> w p -> w q -> w r

instance Gap FreeLeft where
  emptyGap e = FLInternal (Poly (Compose (Sealed e)))
  freeGap e =  FLInternal (Poly (Compose (Sealed e)))
  joinGap op (FLInternal p) (FLInternal q)
    = FLInternal (Poly (case unPoly p of Compose (Sealed p') -> case unPoly q of Compose (Sealed q') -> Compose (Sealed (p' `op` q'))))

instance Gap FreeRight where
  emptyGap e = FRInternal (Poly (FlippedSeal e))
  freeGap e =  FRInternal (Poly (FlippedSeal e))
  joinGap op (FRInternal p) (FRInternal q)
    = FRInternal (Poly (case unPoly q of FlippedSeal q' -> case unPoly p of FlippedSeal p' -> FlippedSeal (p' `op` q')))
