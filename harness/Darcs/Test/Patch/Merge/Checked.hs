module Darcs.Test.Patch.Merge.Checked
  ( CheckedMerge(..), checkedMerger
  ) where

import Darcs.Prelude

import Darcs.Patch.Commute
import Darcs.Patch.CommuteFn
import Darcs.Patch.Effect
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Invert
import Darcs.Patch.Merge
import Darcs.Patch.Named

import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), (:\/:)(..), (:/\:)(..), (+>+), (:>)(..)
    )

import GHC.Stack

class
  (Merge p, Effect p, Eq2 p, Eq2 (PrimOf p), Commute p, Commute (PrimOf p), Invert (PrimOf p))
  => CheckedMerge p
  where

  -- |V1 and V2 merges can produce invalid patches. We use 'checkedMerger' to
  -- validate all merges and fail if there is a problem. When generating tests
  -- we might want to continue after a failure instead of reporting it, so we
  -- can test some other property on all *valid* V1/V2 patches.
  --
  -- This hook allows V1/V2 patches to catch such errors using unsafePerformIO.
  -- Type type is generic to allow arbitrary structures containing mergers to
  -- be checked - e.g. a tuple of two merge results.
  --
  -- There are three reasonable ways of implementing validateMerge. The default
  -- is 'Just' which means no validation.
  --
  -- For repo patch types that might have errors, use unsafePerformIO with try and
  -- evaluate to catch errors and convert them into Nothing.
  --
  -- Finally for compound patch types like Named, FL etc, just delegate to
  -- validateMerge of the underlying patch type.
  --
  -- We could do all this in the Maybe monad right through, but that would
  -- pollute all the generic code with a monad that is only needed because of bugs
  -- in "older" patch implementations
  validateMerge :: a -> Maybe a
  validateMerge = Just

instance CheckedMerge p => CheckedMerge (Named p) where
  validateMerge = validateMerge @p

instance CheckedMerge p => CheckedMerge (FL p) where
  validateMerge = validateMerge @p

checkedMerger :: (HasCallStack, CheckedMerge p) => MergeFn p p -> MergeFn p p
checkedMerger fn pair = let res = fn pair in checkMerge pair res `seq` res

checkMerge
  :: (HasCallStack, CheckedMerge p)
  => (p :\/: p) wX wY
  -> (p :/\: p) wX wY
  -> ()
checkMerge (p :\/: q) (q' :/\: p')
  -- TODO this check doesn't work at the moment - try to enable it and see if it makes
  -- sense to keep or not.
  | False, NotEq <- (p :>: q' :>: NilFL) =\/= (q :>: p' :>: NilFL) =
      error "internal error: merge didn't produce equivalent sequences"
  | NotEq <- squashes (effect p +>+ effect q' +>+ invert (effect q +>+ effect p')) =
      error "internal error: merge didn't produce equivalent effects"
  | otherwise = ()

squashCons :: (Commute p, Eq2 p, Invert p) => p wX wY -> FL p wY wZ -> FL p wX wZ
squashCons p NilFL = p :>: NilFL
squashCons p (q :>: qs)
  | IsEq <- invert p =\/= q = qs
  | Just (q' :> p') <- commute (p :> q) = q' :>: squashCons p' qs
  | otherwise = p :>: q :>: qs

squash :: (Commute p, Eq2 p, Invert p) => FL p wX wY -> FL p wX wY
squash NilFL = NilFL
squash (p :>: ps) = squashCons p (squash ps)

squashes :: (Commute p, Eq2 p, Invert p) => FL p wX wY -> EqCheck wX wY
squashes ps =
  case squash ps of
    NilFL -> IsEq
    _ -> NotEq
