module Darcs.Patch.CommuteFn
    ( CommuteFn,
      commuterIdFL, commuterFLId,
      commuterIdRL, commuterRLId,
      commuterRLFL,
      MergeFn,
      PartialMergeFn,
      mergerIdFL,
      TotalCommuteFn,
      totalCommuterIdFL, totalCommuterFLId, totalCommuterFLFL,
      invertCommuter
    ) where

import Darcs.Prelude

import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , (:\/:)(..)
    , (:/\:)(..)
    , FL(..)
    , RL(..)
    )

-- |CommuteFn is the basis of a general framework for building up commutation
-- operations between different patch types in a generic manner. Unfortunately
-- type classes are not well suited to the problem because of the multiple possible
-- routes by which the commuter for (FL p1, FL p2) can be built out of the
-- commuter for (p1, p2) - and more complicated problems when we start building
-- multiple constructors on top of each other. The type class resolution machinery
-- really can't cope with selecting some route, because it doesn't know that all
-- possible routes should be equivalent.
--
-- Note that a CommuteFn cannot be lazy i.e. commute patches only when the
-- resulting sequences are demanded. This is because of the possibility of
-- failure ('Nothing'): all the commutes must be performed before we can know
-- whether the overall commute succeeds.
type CommuteFn p1 p2 = forall wX wY . (p1 :> p2) wX wY -> Maybe ((p2 :> p1) wX wY)

type TotalCommuteFn p1 p2 = forall wX wY . (p1 :> p2) wX wY -> (p2 :> p1) wX wY

type MergeFn p1 p2 = forall wX wY . (p1 :\/: p2) wX wY -> (p2 :/\: p1) wX wY

type PartialMergeFn p1 p2 = forall wX wY . (p1 :\/: p2) wX wY -> Maybe ((p2 :/\: p1) wX wY)

commuterIdRL :: CommuteFn p1 p2 -> CommuteFn p1 (RL p2)
commuterIdRL _ (x :> NilRL) = return (NilRL :> x)
commuterIdRL commuter (x :> (ys :<: y))
  = do ys' :> x' <- commuterIdRL commuter (x :> ys)
       y' :> x'' <- commuter (x' :> y)
       return ((ys' :<: y') :> x'')

commuterIdFL :: CommuteFn p1 p2 -> CommuteFn p1 (FL p2)
commuterIdFL _ (x :> NilFL) = return (NilFL :> x)
commuterIdFL commuter (x :> (y :>: ys))
  = do y' :> x' <- commuter (x :> y)
       ys' :> x'' <- commuterIdFL commuter (x' :> ys)
       return ((y' :>: ys') :> x'')

-- | TODO document laziness or lack thereof
mergerIdFL :: MergeFn p1 p2 -> MergeFn p1 (FL p2)
mergerIdFL _ (x :\/: NilFL) = NilFL :/\: x
mergerIdFL merger (x :\/: (y :>: ys))
  = case merger (x :\/: y) of
      y' :/\: x' -> case mergerIdFL merger (x' :\/: ys) of
          ys' :/\: x'' -> (y' :>: ys') :/\: x''

-- | TODO document laziness or lack thereof
totalCommuterIdFL :: TotalCommuteFn p1 p2 -> TotalCommuteFn p1 (FL p2)
totalCommuterIdFL _ (x :> NilFL) = NilFL :> x
totalCommuterIdFL commuter (x :> (y :>: ys)) =
   case commuter (x :> y) of
     y' :> x' -> case totalCommuterIdFL commuter (x' :> ys) of
                   ys' :> x'' -> (y' :>: ys') :> x''

commuterFLId :: CommuteFn p1 p2 -> CommuteFn (FL p1) p2
commuterFLId _ (NilFL :> y) = return (y :> NilFL)
commuterFLId commuter ((x :>: xs) :> y)
  = do y' :> xs' <- commuterFLId commuter (xs :> y)
       y'' :> x' <- commuter (x :> y')
       return (y'' :> (x' :>: xs'))

commuterRLId :: CommuteFn p1 p2 -> CommuteFn (RL p1) p2
commuterRLId _ (NilRL :> y) = return (y :> NilRL)
commuterRLId commuter ((xs :<: x) :> y)
  = do y' :> x' <- commuter (x :> y)
       y'' :> xs' <- commuterRLId commuter (xs :> y')
       return (y'' :> (xs' :<: x'))

commuterRLFL :: forall p1 p2. CommuteFn p1 p2 -> CommuteFn (RL p1) (FL p2)
commuterRLFL commuter (xs :> ys) = right xs ys
  where
    right :: RL p1 wX wY -> FL p2 wY wZ -> Maybe ((FL p2 :> RL p1) wX wZ)
    right as NilFL = Just (NilFL :> as)
    right as (b :>: bs) = do
      b' :> as' <- commuterRLId commuter (as :> b)
      bs' :> as'' <- left as' bs
      return (b' :>: bs' :> as'')
    left :: RL p1 wX wY -> FL p2 wY wZ -> Maybe ((FL p2 :> RL p1) wX wZ)
    left NilRL bs = Just (bs :> NilRL)
    left (as :<: a) bs = do
      bs' :> a' <- commuterIdFL commuter (a :> bs)
      bs'' :> as' <- right as bs'
      return (bs'' :> as' :<: a')

-- | TODO document laziness or lack thereof
totalCommuterFLId :: TotalCommuteFn p1 p2 -> TotalCommuteFn (FL p1) p2
totalCommuterFLId _ (NilFL :> y) = y :> NilFL
totalCommuterFLId commuter ((x :>: xs) :> y) =
   case totalCommuterFLId commuter (xs :> y) of
     y' :> xs' -> case commuter (x :> y') of
                    y'' :> x' -> y'' :> (x' :>: xs')

-- | TODO document laziness or lack thereof
totalCommuterFLFL :: TotalCommuteFn p1 p2 -> TotalCommuteFn (FL p1) (FL p2)
totalCommuterFLFL commuter = totalCommuterFLId (totalCommuterIdFL commuter)

-- | Make use of the inverse-commute law to reduce the number of cases
-- when defining commute for complicated patch types.
{-# INLINE invertCommuter #-}
invertCommuter :: (Invert p, Invert q) => CommuteFn p q -> CommuteFn q p
invertCommuter commuter (x :> y) = do
    ix' :> iy' <- commuter (invert y :> invert x)
    return (invert iy' :> invert ix')
