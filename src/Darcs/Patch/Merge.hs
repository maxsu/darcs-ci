-- |
-- Module      : Darcs.Patch.Merge
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Merge
    ( -- * Classes
      CleanMerge(..)
    , Merge(..)
      -- * Functions
    , selfMerger
    , swapMerger
    , mergerIdFL
    , mergerFLId
    , mergerFLFL
    , cleanMergeFL
    , mergeFL
    , swapMerge
    , swapCleanMerge
    , mergeList
      -- * Properties
    , prop_mergeSymmetric
    , prop_mergeCommute
    ) where

import Control.Monad ( foldM )

import Darcs.Prelude

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( MergeFn, PartialMergeFn )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), isIsEq )
import Darcs.Patch.Witnesses.Ordered
    ( (:\/:)(..)
    , (:/\:)(..)
    , FL(..)
    , (:>)(..)
    , (+>+)
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )

{- | Class of patches that can, possibly, be merged cleanly, that is,
without conflict.

Every patch type can be made an instance of 'CleanMerge' in a trivial way by
defining @'cleanMerge' _ = 'Nothing'@, which vacuously conforms to all
required laws.

Instances should obey the following laws:

[/symmetry/]

    prop> cleanMerge (p :\/: q) == Just (q' :/\: p') <=> cleanMerge (q :\/: p) == Just (p' :/\: q')

If an instance @'Commute' p@ exists, then we also require

[/merge-commute/]

    prop> cleanMerge (p :\/: q) == Just (q' :/\: p') ==> commute (p :> q') == Just (q :> p')

    that is, the two branches of a clean merge commute to each other.

If an instance @'Invert' p@ exists, then we also require

[/square-merge/]

    prop> cleanMerge (p :\/: q) == Just (q' :/\: p') => cleanMerge (invert p :\/: q') == Just (q :/\: invert p')

    Here is a picture that explains why we call this /square-merge/:

    >     A---p--->X          A<--p^---X
    >     |        |          |        |
    >     |        |          |        |
    >     q        q'   =>    q        q'
    >     |        |          |        |
    >     v        v          v        v
    >     Y---p'-->B          Y<--p'^--B

-}
class CleanMerge p where
  cleanMerge :: (p :\/: p) wX wY -> Maybe ((p :/\: p) wX wY)

instance CleanMerge p => CleanMerge (FL p) where
  cleanMerge (NilFL :\/: x) = return $ x :/\: NilFL
  cleanMerge (x :\/: NilFL) = return $ NilFL :/\: x
  cleanMerge ((x :>: xs) :\/: ys) = do
    ys' :/\: x' <- cleanMergeFL (x :\/: ys)
    xs' :/\: ys'' <- cleanMerge (ys' :\/: xs)
    return $ ys'' :/\: (x' :>: xs')

-- | Cleanly merge a single patch with an 'FL' of patches.
cleanMergeFL :: CleanMerge p => PartialMergeFn p (FL p)
cleanMergeFL (p :\/: NilFL) = return $ NilFL :/\: p
cleanMergeFL (p :\/: (x :>: xs)) = do
  x' :/\: p'  <- cleanMerge (p :\/: x)
  xs' :/\: p'' <- cleanMergeFL (p' :\/: xs)
  return $ (x' :>: xs') :/\: p''

{- | Patches that can always be merged, even if they conflict.

Instances should obey the following laws:

[/symmetry/]

    prop> merge (p :\/: q) == q' :/\: p' <=> merge (q :\/: p) == p' :/\: q'

[/merge-commute/]

    prop> merge (p :\/: q) == q' :/\: p' ==> commute (p :> q') == Just (q :> p')

    that is, the two branches of a merge commute to each other.

[/extension/]

    prop> cleanMerge (p :\/: q) == Just (q' :/\: p') => merge (p :\/: q) == q' :/\: p'

    that is, 'merge' is an extension of 'cleanMerge'.

-}
class CleanMerge p => Merge p where
    merge :: (p :\/: p) wX wY -> (p :/\: p) wX wY

-- | Synonym for 'merge'.
selfMerger :: Merge p => MergeFn p p
selfMerger = merge

instance Merge p => Merge (FL p) where
    merge = mergerFLFL merge

mergeFL :: Merge p
        => (p :\/: FL p) wX wY
        -> (FL p :/\: p) wX wY
mergeFL = mergerIdFL merge

-- | Lift a merge function over @p :\/: q@
-- to a merge function over @p :\/: FL q@
mergerIdFL :: MergeFn p q -> MergeFn p (FL q)
mergerIdFL _mergeFn (p :\/: NilFL) = NilFL :/\: p
mergerIdFL  mergeFn (p :\/: (x :>: xs)) =
  case mergeFn (p :\/: x) of
    x' :/\: p' -> case mergerIdFL mergeFn (p' :\/: xs) of
      xs' :/\: p'' -> (x' :>: xs') :/\: p''

-- | Lift a merge function over @p :\/: q@
-- to a merge function over @FL p :\/: q@
mergerFLId :: MergeFn p q -> MergeFn (FL p) q
mergerFLId mergeFn = swapMerger (mergerIdFL (swapMerger mergeFn))

-- | Lift a merge function over @p :\/: q@
-- to a merge function over @FL p :\/: FL q@
mergerFLFL :: MergeFn p q -> MergeFn (FL p) (FL q)
mergerFLFL mergeFn = mergerIdFL (mergerFLId mergeFn)

-- | Swap the two patches, 'merge', then swap again. Used to exploit
-- 'prop_mergeSymmetric' when defining 'merge'.
swapMerge :: Merge p => (p :\/: p) wX wY -> (p :/\: p) wX wY
swapMerge = swapMerger merge

-- | Swap the two patches, apply an arbitrary merge function, then swap again.
swapMerger :: MergeFn p q -> MergeFn q p
swapMerger mergeFn (x :\/: y) = case mergeFn (y :\/: x) of x' :/\: y' -> y' :/\: x'

-- | Swap the two patches, 'cleanMerge', then swap again. Used to exploit
-- 'prop_cleanMergeSymmetric' when defining 'cleanMerge'.
swapCleanMerge :: CleanMerge p => (p :\/: p) wX wY -> Maybe ((p :/\: p) wX wY)
swapCleanMerge (x :\/: y) = do
  x' :/\: y' <- cleanMerge (y :\/: x)
  return $ y' :/\: x'

-- | Combine a list of patch sequences, all starting at the same state, into a
-- single sequence that also starts at the same state, using cleanMerge.
-- If the merge fails, we return the two sequences that
-- could not be merged so we can issue more detailed error messages.
mergeList :: CleanMerge p
          => [Sealed (FL p wX)]
          -> Either (Sealed (FL p wX), Sealed (FL p wX)) (Sealed (FL p wX))
mergeList = foldM mergeTwo (Sealed NilFL)
  where
    mergeTwo (Sealed ps) (Sealed qs) =
      case cleanMerge (ps :\/: qs) of
        Just (qs' :/\: _) -> Right $ Sealed $ ps +>+ qs'
        Nothing -> Left (Sealed ps, Sealed qs)

-- | This function serves no purpose except to demonstrate how merge together
-- with the square commute law allows us to commute any pair of adjacent
-- patches.
-- Note that using this function introduces inverse conflictors if the regular
-- commute would fail. This is problematic because it invalidates another
-- global invariant we rely on, namely that we can always drop (obliterate or
-- amend) patches from the end of a repo. This is because inverse conflictors
-- contain references to patches that come after it, so dropping them would
-- make the inverse conflictor inconsistent.
_forceCommute :: (Commute p, Merge p, Invert p) => (p :> p) wX wY -> (p :> p) wX wY
_forceCommute (p :> q) =
  case commute (p :> q) of
    Just (q' :> p') -> q' :> p'
    Nothing ->
      case merge (invert p :\/: q) of
        q' :/\: ip' -> q' :> invert ip'

-- | Whether the given pair of patches satisfies the /symmetry/ law.
prop_mergeSymmetric :: (Eq2 p, Merge p) => (p :\/: p) wX wY -> Bool
prop_mergeSymmetric (p :\/: q) =
  case merge (p :\/: q) of
    q' :/\: p' ->
      case merge (q :\/: p) of
        p'' :/\: q'' ->
          isIsEq (q' =\/= q'') && isIsEq (p' =\/= p'')

-- | Whether the given pair of patches satisfies the /merge-commute/ law.
prop_mergeCommute :: (Commute p, Eq2 p, Merge p) => (p :\/: p) wX wY -> Bool
prop_mergeCommute (p :\/: q) =
  case merge (p :\/: q) of
    q' :/\: p' ->
      case commute (p :> q') of
        Nothing -> False
        Just (q'' :> p'') ->
          isIsEq (q'' =\/= q) && isIsEq (p'' =/\= p')
      &&
      case commute (q :> p') of
        Nothing -> False
        Just (p'' :> q'') ->
          isIsEq (p'' =\/= p) && isIsEq (q'' =/\= q')
