{-# LANGUAGE EmptyDataDecls #-}
module Darcs.Test.Patch.Rebase ( testSuite ) where

import Darcs.Prelude

import Control.Monad ( unless )
import Data.Maybe

import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertFailure )

import Darcs.Patch
import Darcs.Patch.Info
import Darcs.Patch.Named
import Darcs.Patch.Summary
import Darcs.Patch.Rebase.Fixup
import Darcs.Patch.Rebase.Change
import Darcs.Patch.Witnesses.Ordered

import Darcs.Test.Patch.Arbitrary.Generic
import Darcs.Test.TestOnly.Instance ()

import Darcs.Util.Path ( floatPath )

testSuite :: forall p . (RepoPatch p, ArbitraryPrim (PrimOf p)) => [Test]
testSuite =
    if isJust (hasPrimConstruct @(PrimOf p))
        then
           [ duplicateConflictedEffect @p
           ]
        else
           [
           ]

data WX

duplicateConflictedEffect :: forall p . RepoPatch p => Test
duplicateConflictedEffect =
    testCase "duplicate in rebase fixup has a conflicted effect" $
        unless (all (/= Okay) cStatuses) $
            assertFailure ("unexpected conflicted effect: " ++ show cEffect)
    where
        corePrim = addfile (floatPath "file")
        rebase :: RebaseChange (PrimOf p) WX WX
        rebase =
          RC (PrimFixup (invert corePrim) :>: NilFL)
             (NamedP dummyPatchInfo [] (corePrim :>: NilFL))
        dummyPatchInfo = rawPatchInfo "1999" "dummy" "harness" [] False
        cEffect = conflictedEffect rebase
        cStatuses = map (\(IsC status _) -> status) cEffect
