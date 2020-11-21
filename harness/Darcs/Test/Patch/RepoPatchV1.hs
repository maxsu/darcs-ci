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

module Darcs.Test.Patch.RepoPatchV1 ( testSuite ) where

import Darcs.Prelude

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Darcs.Test.Patch.Utils ( testCases )

import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Eq ( unsafeCompare )
import Darcs.Patch.V1 as V1 ( RepoPatchV1 )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim )
import Darcs.Patch.Commute ( Commute(..) )

import Darcs.Test.Patch.Arbitrary.Named ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV1 ()

import qualified Darcs.Test.Patch.Examples.Set1 as Ex

import qualified Darcs.Test.Patch.Properties.V1Set1 as Prop1
import qualified Darcs.Test.Patch.Properties.V1Set2 as Prop2
import qualified Darcs.Test.Patch.Properties.Generic as PropG
import Darcs.Test.Patch.Properties.GenericUnwitnessed ()

import qualified Darcs.Test.Patch.Rebase as Rebase
import qualified Darcs.Test.Patch.Unwind as Unwind

type RPV1 = V1.RepoPatchV1 V1.Prim

unit_V1P1:: [Test]
unit_V1P1 =
  [ testCases "known commutes" Prop1.checkCommute Ex.knownCommutes
  , testCases "known non-commutes" Prop1.checkCantCommute Ex.knownCantCommutes
  , testCases "known merges" Prop1.checkMerge Ex.knownMerges
  , testCases "known merges (equiv)" Prop1.checkMergeEquiv Ex.knownMergeEquivs
  , testCases "known canons" Prop1.checkCanon Ex.knownCanons
  , testCases "merge swaps" Prop1.checkMergeSwap Ex.mergePairs2
  , testCases "the patch validation works" Prop1.tTestCheck Ex.validPatches
  , testCases "commute/recommute" (PropG.recommute commute) Ex.commutePairs
  , testCases "merge properties: merge either way valid" PropG.mergeEitherWayValid Ex.mergePairs
  , testCases "merge properties: merge swap" PropG.mergeEitherWay Ex.mergePairs
  , testCases "primitive patch IO functions" (Prop1.tShowRead eqFLUnsafe) Ex.primitiveTestPatches
  , testCases "IO functions (test patches)" (Prop1.tShowRead eqFLUnsafe) Ex.testPatches
  , testCases "IO functions (named test patches)" (Prop1.tShowRead unsafeCompare) Ex.testPatchesNamed
  , testCases "primitive commute/recommute" (PropG.recommute commute) Ex.primitiveCommutePairs
  ]

qc_V1P1 :: [Test]
qc_V1P1 =
  [
    testProperty "show and read work right" (unseal Prop2.propReadShow)
  ]
  ++ Prop2.checkSubcommutes Prop2.subcommutesInverse "patch and inverse both commute"
  ++ Prop2.checkSubcommutes Prop2.subcommutesNontrivialInverse "nontrivial commutes are correct"
  ++ Prop2.checkSubcommutes Prop2.subcommutesFailure "inverses fail"
  ++
  [ testProperty "commuting by patch and its inverse is ok" Prop2.propCommuteInverse
  -- , testProperty "conflict resolution is valid" Prop.propResolveConflictsValid
  , testProperty "a patch followed by its inverse is identity"
    Prop2.propPatchAndInverseIsIdentity
  , testProperty "'simple smart merge'" Prop2.propSimpleSmartMergeGoodEnough
  , testProperty "commutes are equivalent" Prop2.propCommuteEquivalency
  , testProperty "merges are valid" Prop2.propMergeValid
  , testProperty "inverses being valid" Prop2.propInverseValid
  , testProperty "other inverse being valid" Prop2.propOtherInverseValid
  -- The patch generator isn't smart enough to generate correct test cases for
  -- the following: (which will be obsoleted soon, anyhow)
  -- , testProperty "the order dependence of unravel" Prop.propUnravelOrderIndependent
  -- , testProperty "the unravelling of three merges" Prop.propUnravelThreeMerge
  -- , testProperty "the unravelling of a merge of a sequence" Prop.propUnravelSeqMerge
  , testProperty "the order of commutes" Prop2.propCommuteEitherOrder
  , testProperty "commute either way" Prop2.propCommuteEitherWay
  , testProperty "the double commute" Prop2.propCommuteTwice
  , testProperty "merges commute and are well behaved"
    Prop2.propMergeIsCommutableAndCorrect
  , testProperty "merges can be swapped" Prop2.propMergeIsSwapable
  , testProperty "again that merges can be swapped (I'm paranoid) " Prop2.propMergeIsSwapable
  ]

testSuite :: Test
testSuite =
  testGroup "RepoPatchV1"
    [ testGroup "using V1.Prim wrapper for Prim.V1" $
      unit_V1P1 ++ qc_V1P1 ++
      [ testGroup "Rebase patches" $ Rebase.testSuite @RPV1 ] ++
      [ testGroup "Unwind" $ Unwind.testSuite @RPV1 ]
    ]
