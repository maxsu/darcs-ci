module Darcs.Test.Patch.Unwind
  ( testSuite
  ) where

import Darcs.Prelude

import Darcs.Patch
import Darcs.Patch.Commute
import Darcs.Patch.Unwind
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Show

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Examples.Unwind
import Darcs.Test.Patch.Merge.Checked
import Darcs.Test.Patch.Properties.Generic
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.WithState
import Darcs.Test.Util.TestResult ( TestResult, succeeded, assertNotFailed )

import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- This property could be generalised over all instances of Unwind (not
-- just Named), but in practice it is only interesting for Named, for which
-- the fullUnwind implementation is non-trivial.
propUnwindNamedSucceeds
  :: (Unwind p, PrimPatchBase p)
  => Named p wX wY
  -> TestResult
propUnwindNamedSucceeds p =
  case fullUnwind p of
    Unwound before ps after ->
      lengthFL before `seq` ps `seq` lengthRL after `seq` succeeded

numberedTestCases :: forall a . String -> (a -> TestResult) -> [a] -> [Test]
numberedTestCases text runTest = zipWith numbered [1..]
  where
    numbered :: Int -> a -> Test
    numbered n testItem = testCase (text ++ " " ++ show n) (assertNotFailed $ runTest testItem)

testSuite
  :: forall p
   . ( ArbitraryRepoPatch p, PrimBased p, ArbitraryPrim (OnlyPrim p)
     , ShrinkModel (PrimOf p)
     , Show1 (ModelOf (PrimOf p)), Show2 p
     , CheckedMerge p, Commute (OnlyPrim p)
     )
  => [Test]
testSuite =
    -- TODO these need to take the patch type, currently hard-coded to V1
    numberedTestCases "full unwind example" (withAllSequenceItems propUnwindNamedSucceeds) (examples @p)
      ++
  [ testProperty "unwind named succeeds"
     (withAllSequenceItems (propUnwindNamedSucceeds :: PatchProperty (Named p)))
  ]
