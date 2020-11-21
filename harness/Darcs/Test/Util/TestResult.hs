module Darcs.Test.Util.TestResult
  ( TestResult
  , succeeded
  , failed
  , rejected
  , maybeFailed
  , assertNotFailed
  , isFailed
  ) where

import Darcs.Prelude

import Darcs.Util.Printer (Doc)
import Darcs.Util.Printer.Color (unsafeRenderStringColored)

import qualified Test.QuickCheck.Property as Q
import qualified Test.HUnit as H

-- |Indicate the result of a test, which could be success,
-- failure (with a reason), or that the test couldn't run (rejected),
-- perhaps because the input data didn't meet some pre-condition.
-- The Monoid instance combines results by failing if either result
-- failed, rejecting if both results are rejected, and otherwise
-- succeeding.
data TestResult
  = TestSucceeded
  | TestFailed Doc
  | TestRejected

instance Show TestResult where
  show TestSucceeded = "TestSucceeded"
  show (TestFailed reason) = "TestFailed: " ++ unsafeRenderStringColored reason
  show TestRejected = "TestRejected"

succeeded :: TestResult
succeeded = TestSucceeded

failed :: Doc -> TestResult
failed = TestFailed

rejected :: TestResult
rejected = TestRejected

instance Semigroup TestResult where
  -- Succeed even if one of the arguments is rejected.
  t@(TestFailed _) <> _s = t
  _t <> s@(TestFailed _) = s
  TestRejected <> s = s
  t <> TestRejected = t
  TestSucceeded <> TestSucceeded = TestSucceeded

instance Monoid TestResult where
  mempty = TestRejected
  mappend = (<>)

-- | 'Nothing' is considered success whilst 'Just' is considered failure.
maybeFailed :: Maybe Doc -> TestResult
maybeFailed Nothing = succeeded
maybeFailed (Just errMsg) = failed errMsg

isFailed :: TestResult -> Bool
isFailed (TestFailed _) = True
isFailed _other = False

-- | Convert 'TestResult' to HUnit testable assertion
assertNotFailed :: TestResult -> H.Assertion
assertNotFailed TestSucceeded = return ()
assertNotFailed TestRejected = return ()
assertNotFailed (TestFailed msg) = H.assertString (unsafeRenderStringColored msg)

-- | 'Testable' instance is defined by converting 'TestResult' to
-- 'QuickCheck.Property.Result'
instance Q.Testable TestResult where
  property TestSucceeded = Q.property Q.succeeded
  property (TestFailed errorMsg) =
    Q.property (Q.failed {Q.reason = unsafeRenderStringColored errorMsg})
  property TestRejected = Q.property Q.rejected
