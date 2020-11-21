module Darcs.Patch.CommuteNoConflicts
    ( CommuteNoConflicts(..)
    , mergeNoConflicts
    ) where

import Darcs.Prelude

import Darcs.Patch.Commute ( Commute )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Ordered ( (:/\:)(..), (:>)(..), (:\/:)(..) )

-- | It is natural to think of conflicting patches @p@ and @q@ as
-- a parallel pair @p:\/:q@ because this is how conflicting patches arise.
-- But then Darcs comes along and merges them anyway by converting one of
-- them to a conflictor. Thus, inside a sequence of patches we may see
-- them as a sequential pair @(p:>q')@. In that case, 'commute' will always
-- succeed, as expressed by the 'prop_mergeCommute' law. 'commuteNoConflicts'
-- is a restricted version of 'commute' that should fail in this case but
-- otherwise give the same result as 'commute'.
--
-- Primitive patch types have no conflictors, so for them we have
-- @commute == commuteNoConflicts@.
--
-- Instances should obey the following laws:
--
-- * Symmetry
--
--   prop> commuteNoConflicts (p:>q) == Just (q':>p') <=> commuteNoConflicts (q':>p') == Just (p':>q)
--
-- * Square-Commute (if an instance @'Invert' p@ exists)
--
--   prop> commuteNoConflicts (p:>q) == Just (q':>p') => commuteNoConflicts (invert p:>q') == Just (q:>invert p')
--
-- * 'commuteNoConflicts' is a restriction of 'commute'
--
--   prop> commuteNoConflicts (p:>q) == Just r => commute (p:>q) == Just r
--
class Commute p => CommuteNoConflicts p where
    commuteNoConflicts :: (p :> p) wX wY -> Maybe ((p :> p) wX wY)
    -- ^ An alternative to 'commute' to be used if correctness of your code
    -- depends on the validity of the square-commute law, or to determine
    -- whether patches are in conflict. A parallel pair of patches @p:\/:q@
    -- is conflicting if and only if @commuteNoConflicts(p^:>q)@ fails. Its
    -- main use is so that we can define 'mergeNoConflicts' cleanly.

{- |
The non-conflicting merge of @p:\/:q@ tries to commute the inverse @p^@
of @p@ with @q@. If it succeeds then the part of the result that corresponds
to @p^@ is re-inverted. This is also known as a "clean merge".

Note that to maintain consistency in the presence of conflictors we must use
use 'commuteNoConflicts' here and not 'commute'. Otherwise we run into
contradictions as explained below.

Concretely, suppose we use 'commute' here and that @q@ is a conflictor that
represents the primitive patch @r@ and conflicts (only) with (primitive
patch) @p^@. That is, @q@ results from the conflicted
@merge(r:\/:p^)=(s:/\:q)@, where @s@ is another conflictor. Now, according
to 'prop_mergeCommute' we get @commute(p^:>q)=Just(r:>s)@, and thus
@mergeNoConflict(p:\/:q)=Just(s^:/\:r)@ in contradiction to our assumption
that @p^:\/:q@ are in conflict i.e. @mergeNoConflict(p^:\/:q)@ fails. (This
argument takes for granted that the addition of conflictors to prim patches
preserves their commute behavior. This is not yet stated as a law but all
implementations obviously adhere to it.)

As a side note, the fact that we now get an inverse conflictor @s^@ as part
of the result leads to further problems. For instance, whether our repo is
conflicted now depends on the order of patches: @(p:>r)@ is not conflicted,
but its commute @(q:>s^)@ obviously is. In fact, @(q:>s^)@ is nothing else
but the (identity-preserving) "force-commute" of @(p:>r)@, see the thread at
https://lists.osuosl.org/pipermail/darcs-devel/2017-November/018403.html
-}
mergeNoConflicts :: (Invert p, CommuteNoConflicts p)
                 => (p :\/: p) wX wY -> Maybe ((p :/\: p) wX wY)
mergeNoConflicts (p :\/: q) = do
  q' :> ip' <- commuteNoConflicts (invert p :> q)
  return (q' :/\: invert ip')
