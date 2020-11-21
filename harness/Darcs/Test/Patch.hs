--  Copyright (C) 2002-2005,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

-- UndecidableInstances was added because GHC 8.6 needed it
-- even though GHC 8.2 didn't
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch ( testSuite ) where

import Darcs.Prelude

import Data.Constraint ( Dict(..) )
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck( Arbitrary(..) )

import Darcs.Test.Util.TestResult ( TestResult, maybeFailed )
import Darcs.Test.Patch.Utils
    ( PropList
    , TestCheck(..)
    , TestCondition(..)
    , TestGenerator(..)
    , properties
    , testCases
    , testConditional
    , fromNothing
    )

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq ( Eq2 )
import Darcs.Patch.Witnesses.Show
import Darcs.Patch.Annotate ( Annotate )
import Darcs.Patch.FromPrim ( PrimOf, FromPrim(..) )
import Darcs.Patch.Prim ( PrimPatch, coalesce )
import qualified Darcs.Patch.Prim.FileUUID as FileUUID ( Prim )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import qualified Darcs.Patch.V2.Prim as V2 ( Prim )
import Darcs.Patch.Prim.Named ( NamedPrim )
import Darcs.Patch.V2.RepoPatch ( isConsistent, isForward, RepoPatchV2 )
import Darcs.Patch.V3 ( RepoPatchV3 )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Apply( Apply, ApplyState )
import Darcs.Patch.Merge ( Merge )

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.PatchTree
import Darcs.Test.Patch.Arbitrary.PrimFileUUID()
import Darcs.Test.Patch.Arbitrary.NamedPrimV1 ()
import Darcs.Test.Patch.Arbitrary.NamedPrimFileUUID ()
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV3 ()
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Arbitrary.Shrink ( Shrinkable )
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge )
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState
    ( ArbitraryState
    , PropagateShrink
    , ShrinkModel
    , WithState
    , arbitraryTriple
    , makeS2Gen
    , makeWS2Gen
    , wesPatch
    )

import qualified Darcs.Test.Patch.Info
import qualified Darcs.Test.Patch.Selection

import qualified Darcs.Test.Patch.Examples.Set2Unwitnessed as ExU

import Darcs.Test.Patch.Properties.Check( Check(..) )
import Darcs.Test.Patch.Properties.Generic ( PatchProperty, MergeProperty, SequenceProperty )
import qualified Darcs.Test.Patch.Properties.Generic as PropG
import qualified Darcs.Test.Patch.Properties.RepoPatch as PropR
import qualified Darcs.Test.Patch.Properties.RepoPatchV3 as PropR3
import qualified Darcs.Test.Patch.Properties.GenericUnwitnessed as PropU

import qualified Darcs.Test.Patch.Rebase as Rebase
import qualified Darcs.Test.Patch.Unwind as Unwind

import qualified Darcs.Test.Patch.WSub as WSub


type Prim1 = V1.Prim
type Prim2 = V2.Prim

-- Generic Arbitrary instances

-- We define them here so they don't overlap with those for RepoPatchV1,
-- which use a different generator for V1Model.

type ArbitraryModel p = (RepoModel (ModelOf p), ArbitraryState p)

instance ArbitraryModel p => Arbitrary (Sealed2 (WithState (FL p))) where
  arbitrary = makeWS2Gen aSmallRepo

instance ArbitraryModel p => Arbitrary (Sealed2 (WithState (FL p :> FL p))) where
  arbitrary = makeWS2Gen aSmallRepo

instance ArbitraryModel p => Arbitrary (Sealed2 (FL p :> FL p)) where
  arbitrary = makeS2Gen aSmallRepo

instance ArbitraryModel p => Arbitrary (Sealed2 (FL p)) where
  arbitrary = makeS2Gen aSmallRepo

instance ArbitraryModel p => Arbitrary (Sealed2 (p :> p :> p)) where
  arbitrary = unseal (seal2 . wesPatch) <$> (aSmallRepo >>= arbitraryTriple)

-- End generic instances

unit_V2P1 :: [Test]
unit_V2P1 =
  [ testCases "coalesce commute" (PropU.coalesceCommute WSub.coalesce) ExU.primPermutables
  , testCases "prim recommute" (PropU.recommute WSub.commute) ExU.commutables
  , testCases "square commute law" (PropU.squareCommuteLaw WSub.commute) ExU.commutables
  , testCases "prim inverses commute" (PropU.commuteInverses WSub.commute) ExU.commutables
  , testCases "FL prim recommute" (PropU.recommute WSub.commute) ExU.commutablesFL
  , testCases "FL square commute law" (PropU.squareCommuteLaw WSub.commute) ExU.commutablesFL
  , testCases "FL prim inverses commute" (PropU.commuteInverses WSub.commute) $ ExU.commutablesFL
  , testCases "fails" (PropU.commuteFails WSub.commute) ([] :: [(Prim2 WSub.:> Prim2) wX wY])
  , testCases "read and show work on Prim" PropU.showRead ExU.primPatches
  , testCases "read and show work on RepoPatchV2" PropU.showRead ExU.repov2Patches
  , testCases "example flattenings work" (PropR.propConsistentTreeFlattenings fromPrim2) ExU.repov2PatchLoopExamples
  , testCases "V2 merge input consistent" (PropU.mergeArgumentsConsistent isConsistent) ExU.repov2Mergeables
  , testCases "V2 merge input is forward" (PropU.mergeArgumentsConsistent isForward) ExU.repov2Mergeables
  , testCases "V2 merge output is forward" (PropU.mergeConsistent isForward) ExU.repov2Mergeables
  , testCases "V2 merge output consistent" (PropU.mergeConsistent isConsistent) ExU.repov2Mergeables
  , testCases "V2 merge either way" PropU.mergeEitherWay ExU.repov2Mergeables
  , testCases "V2 merge and commute" PropU.mergeCommute ExU.repov2Mergeables

  , testCases "V2 recommute" (PropU.recommute WSub.commute) ExU.repov2Commutables
  , testCases "V2 inverses commute" (PropU.commuteInverses WSub.commute) ExU.repov2Commutables
  , testCases "V2 permutivity" (PropU.permutivity WSub.commute) ExU.repov2NonduplicateTriples
  ]
  where
    fromPrim2 :: PropR.FromPrimT RepoPatchV2 Prim2
    fromPrim2 = fromAnonymousPrim

arbitraryThing :: TestGenerator thing (Sealed2 thing)
arbitraryThing = TestGenerator (\f p -> Just (unseal2 f p))

qc_prim :: forall prim.
           ( TestablePrim prim
           , Show2 prim
           , Show1 (ModelOf prim)
           , MightBeEmptyHunk prim
           , MightHaveDuplicate prim
           , Arbitrary (Sealed2 prim)
           , Arbitrary (Sealed2 (prim :> prim))
           , Arbitrary (Sealed2 (WithState prim))
           , Arbitrary (Sealed2 (WithState (prim :> prim)))
           ) => [Test]
qc_prim =
  -- The following fails because of setpref patches:
  -- testProperty "prim inverse doesn't commute" (inverseDoesntCommute :: Prim -> Maybe Doc)
  (case runCoalesceTests @prim of
    Just Dict ->
      [ testProperty "prim coalesce effect preserving"
        (unseal2 $ PropG.coalesceEffectPreserving coalesce :: Sealed2 (WithState (prim :> prim)) -> TestResult)
      ]
    Nothing -> [])
    ++ concat
  [ pair_properties         @prim      "arbitrary"    arbitraryThing
  , pair_properties         @(FL prim) "arbitrary FL" arbitraryThing
  , coalesce_properties     @prim      "arbitrary"    arbitraryThing
  , prim_commute_properties @prim      "arbitrary"    arbitraryThing
  , prim_commute_properties @(FL prim) "arbitrary FL" arbitraryThing
  , patch_properties        @prim      "arbitrary"    arbitraryThing
  , patch_properties        @(FL prim) "arbitrary FL" arbitraryThing
  , patch_repo_properties   @prim      "arbitrary"    arbitraryThing
  , patch_repo_properties   @(FL prim) "arbitrary FL" arbitraryThing
  , pair_repo_properties    @prim      "arbitrary"    arbitraryThing
  , pair_repo_properties    @(FL prim) "arbitrary FL" arbitraryThing
  , triple_properties       @prim      "arbitrary"    arbitraryThing
  , [ testProperty "readPatch/showPatch"
      (unseal2 $ PropG.showRead :: Sealed2 prim -> TestResult)
    , testProperty "readPatch/showPatch (FL)"
      (unseal2 $ PropG.showRead :: Sealed2 (FL prim) -> TestResult)
    ]
  ]

qc_named_prim :: forall prim.
                 ( TestablePrim prim
                 , Show2 prim
                 , Show1 (ModelOf (NamedPrim prim))
                 , MightBeEmptyHunk prim
                 , Arbitrary (Sealed2 (NamedPrim prim))
                 , Arbitrary (Sealed2 (NamedPrim prim :> NamedPrim prim))
                 , Arbitrary (Sealed2 (WithState (NamedPrim prim)))
                 , Arbitrary (Sealed2 (WithState (NamedPrim prim :> NamedPrim prim)))
                 ) => [Test]
qc_named_prim =
  qc_prim @(NamedPrim prim) ++
  [ testProperty
      "prim inverse doesn't commute"
      (unseal2 $ PropG.inverseDoesntCommute :: Sealed2 (NamedPrim prim) -> TestResult)
  ]

qc_V2 :: forall prim wXx wYy.
         ( PrimPatch prim
         , Annotate prim
         , Show1 (ModelOf prim)
         , ShrinkModel prim
         , PropagateShrink prim prim
         , ArbitraryPrim prim
         , Shrinkable prim
         , RepoState (ModelOf prim) ~ ApplyState prim
         )
      => prim wXx wYy -> [Test]
qc_V2 _ =
  [ testProperty "with quickcheck that patches are consistent"
    (withSingle consistent)
  ]
  ++ repoPatchProperties @(RepoPatchV2 prim)
  ++ concat
  [ merge_properties   @(RepoPatchV2 prim) "tree" (TestGenerator mergePairFromTree)
  , merge_properties   @(RepoPatchV2 prim) "twfp" (TestGenerator mergePairFromTWFP)
  , pair_properties    @(RepoPatchV2 prim) "tree" (TestGenerator commutePairFromTree)
  , pair_properties    @(RepoPatchV2 prim) "twfp" (TestGenerator commutePairFromTWFP)
  , patch_properties   @(RepoPatchV2 prim) "tree" (TestGenerator patchFromTree)
  , triple_properties  @(RepoPatchV2 prim) "tree" (TestGenerator commuteTripleFromTree)
  ]
  where
    consistent :: RepoPatchV2 prim wX wY -> TestResult
    consistent = maybeFailed . isConsistent

qc_V3 :: forall prim wXx wYy.
         ( PrimPatch prim
         , Annotate prim
         , Show1 (ModelOf prim)
         , ShrinkModel prim
         , PropagateShrink prim prim
         , ArbitraryPrim prim
         , Shrinkable prim
         , RepoState (ModelOf prim) ~ ApplyState prim
         )
      => prim wXx wYy
      -> [Test]
qc_V3 _ =
  [ testProperty "repo invariants"
    (withSequence (PropR3.prop_repoInvariants :: SequenceProperty (RepoPatchV3 prim)))
  ]
  ++ repoPatchProperties @(RepoPatchV3 prim)
  ++ difficultRepoPatchProperties @(RepoPatchV3 prim)

repoPatchProperties :: forall p.
                       ( ArbitraryRepoPatch p
                       , Show2 p
                       , Show1 (ModelOf p)
                       , CheckedMerge p
                       , ShrinkModel (PrimOf p)
                       , PrimBased p
                       )
                    => [Test]
repoPatchProperties =
  [ testProperty "readPatch/showPatch"
      (withSingle (PropG.showRead :: PatchProperty p))
  , testProperty "readPatch/showPatch (RL)"
      (withSequence (PropG.showRead :: SequenceProperty p))
{- we no longer support inversion for RepoPatches
  , testProperty "invert involution"
      (withSingle (PropG.invertInvolution :: PatchProperty p))
  , testProperty "inverse composition"
      (withPair (PropG.inverseComposition :: PairProperty p))
-}
  , testProperty "resolutions don't conflict"
      (withSequence (PropR.propResolutionsDontConflict :: SequenceProperty p))
  ]

-- | These properties regularly fail for RepoPatchV2 with the standard test
-- case generator when we crank up the number of tests (to e.g. 10000).
difficultRepoPatchProperties :: forall p.
                       ( ArbitraryRepoPatch p
                       , ShrinkModel (PrimOf p)
                       , Show2 p
                       , CheckedMerge p
                       , MightHaveDuplicate p
                       , Show1 (ModelOf p)
                       , PrimBased p
                       )
                    => [Test]
difficultRepoPatchProperties =
  [ testProperty "reorderings are consistent"
      (PropR.propConsistentReorderings @p)
{- we no longer support inversion for RepoPatches
  , testProperty "inverses commute"
      (withPair (PropG.commuteInverses com))
  , testConditional "nontrivial inverses commute"
      (withPair nontrivialCommute)
      (withPair (PropG.commuteInverses com))
-}
  , testProperty "recommute"
      (withPair (PropG.recommute com))
  , testConditional "nontrivial recommute"
      (fromNothing . withPair nontrivialCommute)
      (withPair (PropG.recommute com))
  , testConditional "permutivity"
      (fromNothing . withTriple notDuplicatestriple)
      (withTriple (PropG.permutivity com))
  , testConditional "nontrivial permutivity"
      (fromNothing . withTriple (\t -> nontrivialTriple t && notDuplicatestriple t))
      (withTriple (PropG.permutivity com))
  , testProperty "merge either way"
      (withFork (PropG.mergeEitherWay :: MergeProperty p))
{- this test relies on inversion and is thereore only valid for prims
  , testProperty "merge either way valid"
      (withFork (PropG.mergeEitherWayValid :: MergeProperty p))
-}
  , testConditional "nontrivial merge either way"
      (fromNothing . withFork nontrivialMerge)
      (withFork (PropG.mergeEitherWay :: MergeProperty p))
  , testProperty "merge commute"
      (withFork (PropG.mergeCommute :: MergeProperty p))
  , testProperty "resolutions are invariant under reorderings"
      (withSequence (PropR.propResolutionsOrderIndependent :: SequenceProperty p))
  ]
  where
    com :: (p :> p) wA wB -> Maybe ((p :> p) wA wB)
    com = commute

pair_properties :: forall p gen
                 . ( Show gen, Arbitrary gen, MightHaveDuplicate p
                   , Commute p, Invert p, ShowPatchBasic p, Eq2 p
                   )
                => PropList (p :> p) gen
pair_properties genname gen =
  properties gen "commute" genname
  [ ("recommute"              , TestCondition (const True)     , TestCheck (PropG.recommute commute)           )
  , ("nontrivial recommute"   , TestCondition nontrivialCommute, TestCheck (PropG.recommute commute)           )
  , ("inverses commute"       , TestCondition (const True)     , TestCheck (PropG.commuteInverses commute)     )
  , ("nontrivial inverses"    , TestCondition nontrivialCommute, TestCheck (PropG.commuteInverses commute)     )
  , ("inverse composition"    , TestCondition (const True)     , TestCheck PropG.inverseComposition            )
  ]

coalesce_properties :: forall p gen
                     . ( Show gen, Arbitrary gen, TestablePrim p
                       , MightBeEmptyHunk p
                       )
                    => PropList (p :> p :> p) gen
coalesce_properties genname gen =
  properties gen "commute" genname
   (case runCoalesceTests @p of
      Just Dict -> [ ("coalesce commutes with commute", TestCondition (const True), TestCheck (PropG.coalesceCommute coalesce)) ]
      Nothing -> [])

-- The following properties do not hold for "RepoPatchV2" patches (conflictors and
-- duplicates, specifically) .
prim_commute_properties :: forall p gen
                            . (Show gen, Arbitrary gen, Commute p, Invert p, ShowPatchBasic p, Eq2 p)
                           => PropList (p :> p) gen
prim_commute_properties genname gen =
  properties gen "commute" genname
  [ ("square commute law", TestCondition (const True)     , TestCheck (PropG.squareCommuteLaw commute))
  , ("nontrivial square commute law", TestCondition nontrivialCommute, TestCheck (PropG.squareCommuteLaw commute))
  ]

patch_properties :: forall p gen .
                    ( Show gen
                    , Arbitrary gen
                    , Invert p
                    , Eq2 p
                    , ShowPatchBasic p
                    )
                 => PropList p gen
patch_properties genname gen =
  properties gen "patch" genname
  [ ("inverse . inverse is id"  , TestCondition (const True)     , TestCheck PropG.invertInvolution)
  ]

patch_repo_properties
  :: forall p gen
   . ( Show gen, Arbitrary gen
     , Invert p, Apply p, ShowPatchBasic p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     )
  =>  PropList (WithState p) gen
patch_repo_properties genname gen =
  properties gen "patch/repo" genname
  [ ("invert rollback"          , TestCondition (const True)     , TestCheck PropG.invertRollback)
  ]

merge_properties :: forall p gen .
                    ( Show gen, Arbitrary gen, Commute p
                    , Invert p, Eq2 p, Merge p, ShowPatchBasic p
                    , MightHaveDuplicate p, Check p
                    )
                 => PropList (p :\/: p) gen
merge_properties genname gen =
  properties gen "merge" genname
  [ ("merge either way"           , TestCondition (const True)   , TestCheck PropG.mergeEitherWay      )
  , ("merge either way valid"     , TestCondition (const True)   , TestCheck PropG.mergeEitherWayValid )
  , ("nontrivial merge either way", TestCondition nontrivialMerge, TestCheck PropG.mergeEitherWay      )
  , ("merge commute"              , TestCondition (const True)   , TestCheck PropG.mergeCommute        )
  ]

triple_properties :: forall p gen .
                     ( Show gen, Arbitrary gen, Commute p
                     , Eq2 p, ShowPatchBasic p
                     , MightHaveDuplicate p
                     )
                  => PropList (p :> p :> p) gen
triple_properties genname gen =
  properties gen "triple" genname
  [ ( "permutivity"
    , TestCondition (notDuplicatestriple)
    , TestCheck (PropG.permutivity commute) )
  , ( "nontrivial permutivity"
    , TestCondition (\t -> nontrivialTriple t && notDuplicatestriple t)
    , TestCheck (PropG.permutivity commute) )
  ]

pair_repo_properties
  :: forall p gen .
     ( Show gen
     , Arbitrary gen
     , Commute p
     , Apply p
     , ShowPatchBasic p
     , MightBeEmptyHunk p
     , RepoModel (ModelOf p)
     , RepoState (ModelOf p) ~ ApplyState p
     )
  => PropList (WithState (p :> p)) gen
pair_repo_properties genname gen =
  properties gen "patch/repo" genname
    [ ( "commute is effect preserving"
      , TestCondition (const True)
      , TestCheck (PropG.effectPreserving commute))
    ]

-- tests (either QuickCheck or Unit) that should be run on any type of patch
general_patchTests
  :: forall p
   . ( ArbitraryRepoPatch p, CheckedMerge p
     , PrimBased p, Commute (OnlyPrim p), ArbitraryPrim (OnlyPrim p)
     , ShrinkModel (PrimOf p)
     , Show1 (ModelOf (PrimOf p)), Show2 p
     )
  => [Test]
general_patchTests =
     [ testGroup "Rebase patches" $ Rebase.testSuite @p
     , testGroup "Unwind" $ Unwind.testSuite @p
     ]

-- | This is the big list of tests that will be run using testrunner.
testSuite :: [Test]
testSuite =
    [ primTests
    , repoPatchV2Tests
    , repoPatchV3Tests
    , Darcs.Test.Patch.Info.testSuite
    , Darcs.Test.Patch.Selection.testSuite
    ]
  where
    primTests = testGroup "Prim patches"
      [ testGroup "V1.Prim wrapper for Prim.V1" $ qc_prim @Prim1
      , testGroup "V2.Prim wrapper for Prim.V1" $ qc_prim @Prim2
      , testGroup "Prim.FileUUID" $ qc_prim @FileUUID.Prim
      , testGroup "NamedPrim over V2.Prim" $ qc_named_prim @Prim2
      , testGroup "NamedPrim over Prim.FileUUID" $ qc_named_prim @FileUUID.Prim
      ]
    repoPatchV2Tests = testGroup "RepoPatchV2"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          unit_V2P1 ++ qc_V2 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV2 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V2 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV2 FileUUID.Prim)
      ]
    repoPatchV3Tests = testGroup "RepoPatchV3"
      [ testGroup "using V2.Prim wrapper for Prim.V1" $
          qc_V3 (undefined :: Prim2 wX wY) ++
          general_patchTests @(RepoPatchV3 Prim2)
      , testGroup "using Prim.FileUUID" $
          qc_V3 (undefined :: FileUUID.Prim wX wY) ++
          general_patchTests @(RepoPatchV3 FileUUID.Prim)
      ]
