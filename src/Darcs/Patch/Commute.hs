module Darcs.Patch.Commute
    ( Commute(..)
    , commuteFL
    , commuteRL
    , commuteRLFL
    , selfCommuter
    ) where

import Darcs.Prelude

import Darcs.Patch.CommuteFn
    ( CommuteFn
    , commuterIdFL
    , commuterRLId
    , commuterRLFL
    )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseFL, reverseRL,
    (:>)(..) )

-- | Commute represents things that can be (possibly) commuted.
--
-- Instances should obey the following laws:
--
-- * Symmetry
--
--   prop> commute (p:>q) == Just (q':>p') <=> commute (q':>p') == Just (p':>q)
--
-- * If an instance @'Invert' p@ exists, then
--
--   prop> commute (p:>q) == Just (q':>p') <=> commute (invert q:>invert p) == Just (invert p':>invert q')
--
-- * The more general Square-Commute law
--
--   prop> commute (p:>q) == Just (q':>p') => commute (invert p:>q') == Just (q:>invert p')
--
--   is required to hold only for primitive patches, i.e. if there is /no/
--   instance @'Merge' p@, because together with 'merge' it implies that
--   any two patches commute.
class Commute p where
    commute :: (p :> p) wX wY -> Maybe ((p :> p) wX wY)

instance Commute p => Commute (FL p) where
    {-# INLINE commute #-}
    commute (NilFL :> x) = Just (x :> NilFL)
    commute (x :> NilFL) = Just (NilFL :> x)
    commute (xs :> ys) = do
        ys' :> rxs' <- commuteRLFL (reverseFL xs :> ys)
        return $ ys' :> reverseRL rxs'

-- |'commuteRLFL' commutes an 'RL' past an 'FL'.
{-# INLINE commuteRLFL #-}
commuteRLFL :: Commute p => (RL p :> FL p) wX wY
            -> Maybe ((FL p :> RL p) wX wY)
commuteRLFL = commuterRLFL commute

instance Commute p => Commute (RL p) where
    {-# INLINE commute #-}
    commute (xs :> ys) = do
        fys' :> xs' <- commuteRLFL (xs :> reverseRL ys)
        return (reverseFL fys' :> xs')

-- |'commuteRL' commutes a RL past a single element.
{-# INLINE commuteRL #-}
commuteRL :: Commute p => (RL p :> p) wX wY -> Maybe ((p :> RL p) wX wY)
commuteRL = commuterRLId commute

-- |'commuteFL' commutes a single element past a FL.
{-# INLINE commuteFL #-}
commuteFL :: Commute p => (p :> FL p) wX wY -> Maybe ((FL p :> p) wX wY)
commuteFL = commuterIdFL commute

-- |Build a commuter between a patch and itself using the operation from the type class.
selfCommuter :: Commute p => CommuteFn p p
selfCommuter = commute
