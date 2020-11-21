{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Darcs.Prelude

import Control.Arrow ( second )
import Data.Maybe ( fromMaybe )
import Data.Map ( elems, fromListWith, mapWithKey )

import qualified Data.ByteString as B (ByteString, empty)

import System.FilePath ( (</>) )

import Darcs.Patch.Prim.Class ( PrimCanonize(..) )
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..)
    , comparePrim, isIdentity
    )
import Darcs.Patch.Prim.V1.Show ()
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (:>)(..)
    , reverseRL, mapFL, mapFL_FL
    , concatFL, lengthFL, (+>+) )
import Darcs.Patch.Witnesses.Sealed
    ( unseal, Sealed2(..), unseal2
    , Gap(..), unFreeLeft
    )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Patch.Invert ( Invert(..), dropInverses )
import Darcs.Patch.Commute ( Commute(..) )

import Darcs.Util.Diff ( getChanges )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm )
import Darcs.Util.Global ( darcsdir )
import Darcs.Util.Path ( AnchoredPath, floatPath )

mapPrimFL :: (forall wX wY . FL Prim wX wY -> FL Prim wX wY)
             -> FL Prim wW wZ -> FL Prim wW wZ
mapPrimFL f x =
-- an optimisation; break the list up into independent sublists
-- and apply f to each of them
     case mapM toSimpleSealed $ mapFL Sealed2 x of
     Just sx -> concatFL $ unsealList $ elems $
                mapWithKey (\ k p -> Sealed2 (f (fromSimples k (unsealList (p []))))) $
                fromListWith (flip (.)) $
                map (\ (a,b) -> (a,(b:))) sx
     Nothing -> f x
  where
        unsealList :: [Sealed2 p] -> FL p wA wB
        unsealList = foldr ((:>:) . unseal2 unsafeCoerceP) (unsafeCoerceP NilFL)

        toSimpleSealed :: Sealed2 Prim -> Maybe (AnchoredPath, Sealed2 Simple)
        toSimpleSealed (Sealed2 p) = fmap (second Sealed2) (toSimple p)

data Simple wX wY
    = SFP !(FilePatchType wX wY)
    | SDP !(DirPatchType wX wY)
    | SCP String String String
    deriving ( Show )

toSimple :: Prim wX wY -> Maybe (AnchoredPath, Simple wX wY)
toSimple (FP a b) = Just (a, SFP b)
toSimple (DP a AddDir) = Just (a, SDP AddDir)
toSimple (DP _ RmDir) = Nothing -- ordering is trickier with rmdir present
toSimple (Move _ _) = Nothing
toSimple (ChangePref a b c) = Just (floatPath (darcsdir </> "prefs" </> "prefs"), SCP a b c)

fromSimple :: AnchoredPath -> Simple wX wY -> Prim wX wY
fromSimple a (SFP b) = FP a b
fromSimple a (SDP b) = DP a b
fromSimple _ (SCP a b c) = ChangePref a b c

fromSimples :: AnchoredPath -> FL Simple wX wY -> FL Prim wX wY
fromSimples a = mapFL_FL (fromSimple a)

tryHarderToShrink :: FL Prim wX wY -> FL Prim wX wY
tryHarderToShrink x = tryToShrink2 $ fromMaybe x (dropInverses x)

tryToShrink2 :: FL Prim wX wY -> FL Prim wX wY
tryToShrink2 psold =
    let ps = sortCoalesceFL psold
        ps_shrunk = shrinkABit ps
                    in
    if lengthFL ps_shrunk < lengthFL ps
    then tryToShrink2 ps_shrunk
    else ps_shrunk

-- | @shrinkABit ps@ tries to simplify @ps@ by one patch,
--   the first one we find that coalesces with its neighbour
shrinkABit :: FL Prim wX wY -> FL Prim wX wY
shrinkABit NilFL = NilFL
shrinkABit (p:>:ps) = fromMaybe (p :>: shrinkABit ps) $ tryOne NilRL p ps

-- | @tryOne acc p ps@ pushes @p@ as far down @ps@ as we can go
--   until we can either coalesce it with something or it can't
--   go any further.  Returns @Just@ if we manage to get any
--   coalescing out of this
tryOne :: RL Prim wW wX -> Prim wX wY -> FL Prim wY wZ
        -> Maybe (FL Prim wW wZ)
tryOne _ _ NilFL = Nothing
tryOne sofar p (p1:>:ps) =
    case coalesceOrCancel p p1 of
    Just p' -> Just (reverseRL sofar +>+ p' +>+ ps)
    Nothing -> case commute (p :> p1) of
               Nothing -> Nothing
               Just (p1' :> p') -> tryOne (sofar:<:p1') p' ps

-- | The heart of "sortCoalesceFL"
sortCoalesceFL2 :: FL Prim wX wY -> FL Prim wX wY
sortCoalesceFL2 NilFL = NilFL
sortCoalesceFL2 (x:>:xs) | IsEq <- isIdentity x = sortCoalesceFL2 xs
sortCoalesceFL2 (x:>:xs) = either id id $ pushCoalescePatch x $ sortCoalesceFL2 xs

-- | 'pushCoalescePatch' @new ps@ is almost like @new :>: ps@ except
--   as an alternative to consing, we first try to coalesce @new@ with
--   the head of @ps@.  If this fails, we try again, using commutation
--   to push @new@ down the list until we find a place where either
--   (a) @new@ is @LT@ the next member of the list [see 'comparePrim']
--   (b) commutation fails or
--   (c) coalescing succeeds.
--   The basic principle is to coalesce if we can and cons otherwise.
--
--   As an additional optimization, pushCoalescePatch outputs a Left
--   value if it wasn't able to shrink the patch sequence at all, and
--   a Right value if it was indeed able to shrink the patch sequence.
--   This avoids the O(N) calls to lengthFL that were in the older
--   code.
--
--   Also note that pushCoalescePatch is only ever used (and should
--   only ever be used) as an internal function in in
--   sortCoalesceFL2.
pushCoalescePatch :: Prim wX wY -> FL Prim wY wZ
                    -> Either (FL Prim wX wZ) (FL Prim wX wZ)
pushCoalescePatch new NilFL = Left (new:>:NilFL)
pushCoalescePatch new ps@(p:>:ps')
    = case coalesceOrCancel new p of
      Just (new' :>: NilFL) -> Right $ either id id $ pushCoalescePatch new' ps'
      Just NilFL -> Right ps'
      Just _ -> error "impossible case" -- coalesce either returns a singleton or empty
      Nothing -> if comparePrim new p == LT then Left (new:>:ps)
                            else case commute (new :> p) of
                                 Just (p' :> new') ->
                                     case pushCoalescePatch new' ps' of
                                     Right r -> Right $ either id id $
                                                pushCoalescePatch p' r
                                     Left r -> Left (p' :>: r)
                                 Nothing -> Left (new:>:ps)

coalesceOrCancel :: Prim wX wY -> Prim wY wZ -> Maybe (FL Prim wX wZ)
coalesceOrCancel p1 p2
  | IsEq <- invert p1 =\/= p2 = Just NilFL
  | otherwise = fmap (:>: NilFL) $ coalescePair p1 p2

-- | @'coalescePair' p1 p2@ tries to combine @p1@ and @p2@ into a single
--   patch. For example, two hunk patches
--   modifying adjacent lines can be coalesced into a bigger hunk patch.
--   Or a patch which moves file A to file B can be coalesced with a
--   patch that moves file B into file C, yielding a patch that moves
--   file A to file C.
coalescePair :: Prim wX wY -> Prim wY wZ -> Maybe (Prim wX wZ)
coalescePair (FP f1 p1) (FP f2 p2)
  | f1 /= f2 = Nothing
  | otherwise = coalesceFilePrim f1 p1 p2
coalescePair (Move a b) (Move b' c) | b == b' = Just $ Move a c
coalescePair (FP a AddFile) (Move a' b) | a == a' = Just $ FP b AddFile
coalescePair (DP a AddDir) (Move a' b) | a == a' = Just $ DP b AddDir
coalescePair (Move a b) (FP b' RmFile) | b == b' = Just $ FP a RmFile
coalescePair (Move a b) (DP b' RmDir) | b == b' = Just $ DP a RmDir
{- we don't want to do that, of course:
coalescePair (FP a RmFile) (FP b AddFile) | a == a' = Just $ Move a' b
coalescePair (DP a RmDir) (DP b AddDir) | a == a' = Just $ Move a' b
-}
coalescePair (ChangePref p a b) (ChangePref p' b' c)
  | p == p' && b == b' = Just $ ChangePref p a c
coalescePair _ _ = Nothing

-- | If 'coalescePair' is "addition" then this is "subtraction".
decoalescePair :: Prim wX wZ -> Prim wX wY -> Maybe (Prim wY wZ)
-- These two cases make sense only if we decoalesce;
-- they correspond to the commented two cases for coalesce above
-- and are one reason we need to define this function as a primitive
decoalescePair (Move a b) (FP b' AddFile) | b == b' = Just (FP a RmFile)
decoalescePair (Move a b) (DP b' AddDir) | b == b' = Just (DP a RmDir)
decoalescePair (FP f1 p1) (FP f2 p2)
  | f1 /= f2 = Nothing
  | otherwise = decoalesceFilePrim f1 p1 p2
decoalescePair z x = coalescePair (invert x) z

coalesceFilePrim :: AnchoredPath -> FilePatchType wX wY -> FilePatchType wY wZ
                 -> Maybe (Prim wX wZ)
coalesceFilePrim f (Hunk line1 old1 new1) (Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
-- Token replace patches operating right after (or before) AddFile (RmFile)
-- is an identity patch, as far as coalescing is concerned.
-- These two cases make no sense when we decoalesce, which is the second
-- reason decoalesce is defined as a primitive.
coalesceFilePrim f (AddFile) (TokReplace{}) = Just $ FP f AddFile
coalesceFilePrim f (TokReplace{}) (RmFile) = Just $ FP f RmFile
coalesceFilePrim f (TokReplace t1 a b) (TokReplace t2 b' c)
    | t1 == t2 && b == b' = Just $ FP f $ TokReplace t1 a c
coalesceFilePrim f (Binary o m') (Binary m n)
    | m == m' = Just $ FP f $ Binary o n
coalesceFilePrim _ _ _ = Nothing

decoalesceFilePrim :: AnchoredPath -> FilePatchType wX wZ -> FilePatchType wX wY
                   -> Maybe (Prim wY wZ)
-- These two cases must fail because the token replace patches that coalesce
-- has eliminated are irretrievably lost.
decoalesceFilePrim _ (AddFile) (RmFile) = Nothing
decoalesceFilePrim _ (RmFile) (TokReplace{}) = Nothing
decoalesceFilePrim f z x = coalesceFilePrim f (invert x) z

coalesceHunk :: AnchoredPath
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Maybe (Prim wX wY)
coalesceHunk f line1 old1 new1 line2 old2 new2
    | line2 == line1 && lengthold2 < lengthnew1 =
        if take lengthold2 new1 /= old2
        then Nothing
        else case drop lengthold2 new1 of
        extranew -> Just (FP f (Hunk line2 old1 (new2 ++ extranew)))
    | line2 == line1 && lengthold2 > lengthnew1 =
        if take lengthnew1 old2 /= new1
        then Nothing
        else case drop lengthnew1 old2 of
        extraold -> Just (FP f (Hunk line2 (old1 ++ extraold) new2))
    | line2 == line1 = if new1 == old2 then Just (FP f (Hunk line2 old1 new2))
                       else Nothing
    | line2 < line1 && lengthold2 >= line1 - line2 =
        case take (line1 - line2) old2 of
        extra-> coalesceHunk f line2 (extra ++ old1) (extra ++ new1) line2 old2 new2
    | line2 > line1 && lengthnew1 >= line2 - line1 =
        case take (line2 - line1) new1 of
        extra-> coalesceHunk f line1 old1 new1 line1 (extra ++ old2) (extra ++ new2)
    | otherwise = Nothing
    where lengthold2 = length old2
          lengthnew1 = length new1

canonizeHunk :: Gap w
             => D.DiffAlgorithm -> AnchoredPath -> Int -> [B.ByteString] -> [B.ByteString]
             -> w (FL Prim)
canonizeHunk _ f line old new
    | null old || null new || old == [B.empty] || new == [B.empty]
        = freeGap (FP f (Hunk line old new) :>: NilFL)
canonizeHunk da f line old new = makeHoley f line $ getChanges da old new

makeHoley :: Gap w
          => AnchoredPath -> Int -> [(Int,[B.ByteString], [B.ByteString])]
          -> w (FL Prim)
makeHoley f line =
    foldr (joinGap (:>:) . (\(l,o,n) -> freeGap (FP f (Hunk (l+line) o n)))) (emptyGap NilFL)

instance PrimCanonize Prim where
   tryToShrink = mapPrimFL tryHarderToShrink

   sortCoalesceFL = mapPrimFL sortCoalesceFL2
   canonize _ p | IsEq <- isIdentity p = NilFL
   canonize da (FP f (Hunk line old new)) = unseal unsafeCoercePEnd $ unFreeLeft $ canonizeHunk da f line old new
   canonize _ p = p :>: NilFL
   -- Note: it is important to first coalesce and then canonize, since
   -- coalescing can produce non-cononical hunks (while hunks resulting
   -- from canonizing a single hunk cannot be coalesced). See issue525,
   -- in particular msg20270 for details.
   canonizeFL da = concatFL . mapFL_FL (canonize da) . sortCoalesceFL
   coalesce (p1 :> p2) = coalesceOrCancel p1 p2
   primCoalesce = coalescePair
   primDecoalesce = decoalescePair
