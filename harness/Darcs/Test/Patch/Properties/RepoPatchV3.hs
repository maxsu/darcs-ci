{-# LANGUAGE PatternSynonyms #-}
module Darcs.Test.Patch.Properties.RepoPatchV3
    ( prop_repoInvariants
    ) where

import Prelude ()
import Darcs.Prelude
import qualified Data.Set as S

import Darcs.Test.Util.TestResult ( TestResult, succeeded, failed )
import Darcs.Test.TestOnly.Instance ()

import Darcs.Patch.Commute
import Darcs.Patch.Ident
import Darcs.Patch.Invert
import Darcs.Patch.Permutations ( headPermutationsRL )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Show ( displayPatch, ShowPatchFor(..) )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.V3 ( RepoPatchV3 )
import Darcs.Patch.V3.Contexted
import Darcs.Patch.V3.Core ( pattern PrimP, pattern ConflictorP )

import Darcs.Util.Printer

-- * Repo Invariants

-- What we mean with "repo" here is a flattened version i.e. with named patches
-- replaced by their content, disregarding explicit dependencies. This is why
-- we represent them with a plain 'RL' of 'RepoPatchV3'.

prop_repoInvariants :: PrimPatch p => RL (RepoPatchV3 p) wX wY -> TestResult
prop_repoInvariants NilRL = succeeded
prop_repoInvariants (ps :<: p) =
    prop_repoInvariants ps <>
    prop_positiveId p <>
    prop_uniqueId ps p <>
    prop_consistentConflictor p <>
    prop_onlyFirstConflictorReverts ps p <>
    prop_conflictsCommutePastConflictor ps p <>
    prop_containedCtxEq (ps :<: p)
  where
    -- each patch in a repo has a positive identity
    prop_positiveId x
      | positiveId (ident x) = succeeded
      | otherwise = failed $ text "prop_positiveId"
    -- each patch in a repo has a unique identity
    prop_uniqueId xs x
      | ident x `notElem` mapRL ident xs = succeeded
      | otherwise = failed $ text "prop_uniqueId"

prop_consistentConflictor :: (Invert prim, Commute prim)
                          => RepoPatchV3 prim wX1 wX2 -> TestResult
prop_consistentConflictor (ConflictorP _ x p)
  | all prop_ctxInvariants (p : S.toList x)
  , prop_ctxPositive p
  , all prop_ctxPositive x = succeeded
  | otherwise = failed $ text "prop_consistentConflictor"
prop_consistentConflictor _ = succeeded

-- | This property states that a 'Conflictor' reverts only prims that have not
-- already been reverted by any earlier 'Conflictor'. In other words, the set
-- of 'PatchId's of reverted prims does not intersect with the set of those of
-- preceding 'Conflictor'.
prop_onlyFirstConflictorReverts :: PrimPatch p
                                => RL (RepoPatchV3 p) wX wY
                                -> RepoPatchV3 p wY wZ
                                -> TestResult
prop_onlyFirstConflictorReverts ps p
  | S.null doubly_reverted = succeeded
  | otherwise = failed
      $ text "undone patches are already undone:"
      $$ vcat (map (showId ForStorage) (S.toList doubly_reverted))
      $$ text "in the sequence:"
      $$ vcat (mapRL displayPatch (ps :<: p))
  where
    doubly_reverted = S.intersection this_rids preceding_rids
    this_rids = revertedIds p
    preceding_rids = S.unions (mapRL revertedIds ps)
    revertedIds (ConflictorP r _ _) = S.map invertId (idsFL r)
    revertedIds _ = S.empty

-- | This property states that the patches that a conflictor at the
-- end of a repo conflicts with are in the patches preceding it,
-- that these patches together commute past the conflictor, thereby
-- turning the conflictor into a 'Prim' patch.
-- Note that this does not mean that each of them separately commutes
-- past the conflictor, since there may be dependencies among them.
prop_conflictsCommutePastConflictor :: PrimPatch p
                                    => RL (RepoPatchV3 p) wX wY
                                    -> RepoPatchV3 p wY wZ
                                    -> TestResult
prop_conflictsCommutePastConflictor ps p
  | not (xids `S.isSubsetOf` rids)
  = failed
      $ text "conflicting patches not found in repo:"
      $$ vcat (mapRL displayPatch (ps :<: p))
  | not (revertedIds p `S.isSubsetOf` rids)
  = failed
      $ text "undone patches not found in repo:"
      $$ vcat (mapRL displayPatch (ps :<: p))
  | otherwise =
      case commuteWhatWeCanToPostfix xids ps of
        _ :> xs ->
          case commuteRL (xs :> p) of
            Just (PrimP _ :> _) -> succeeded
            Just _ ->
              failed
                $ text "commuting conflicts past conflictor does not result in a Prim:"
                $$ displayPatch (ps :<: p)
            Nothing ->
              failed
                $ text "cannot commute conflicts past conflictor:"
                $$ displayPatch (ps :<: p)
  where
    xids = conflictIds p
    rids = idsRL ps
    conflictIds (ConflictorP _ x _) = S.map ctxId x
    conflictIds _ = S.empty
    revertedIds (ConflictorP r _ _) = S.map invertId (idsFL r)
    revertedIds _ = S.empty

-- | This is 'prop_ctxEq' checked for any pair of 'Contexted' patches
-- from an 'RL' of 'RepoPatchV3' that we can bring into a common context.
prop_containedCtxEq :: PrimPatch p => RL (RepoPatchV3 p) wX wY -> TestResult
prop_containedCtxEq =
    allSucceeded . map propCtxEq . pairs . concatMap contextedIn . headPermutationsRL
  where
    pairs :: [a] -> [(a,a)]
    pairs xs = [(x,y) | x <- xs, y <- xs]
    contextedIn (_ :<: ConflictorP _ x p) = p : S.toList x
    contextedIn _ = []
    propCtxEq (cp, cq)
      | prop_ctxEq cp cq = succeeded
      | otherwise =
          failed
          $ text "prop_ctxEq: cp="
          $$ showCtx ForStorage cp
          $$ text "cq="
          $$ showCtx ForStorage cq
    allSucceeded = foldr (<>) succeeded

idsFL :: Ident p => FL p wX wY -> S.Set (PatchId p)
idsFL = S.fromList . mapFL ident

idsRL :: Ident p => RL p wX wY -> S.Set (PatchId p)
idsRL = S.fromList . mapRL ident
