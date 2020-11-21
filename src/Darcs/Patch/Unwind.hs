-- BSD3
module Darcs.Patch.Unwind
  ( Unwind(..)
  , Unwound(..)
  , mkUnwound
  , squashUnwound
  ) where

import Darcs.Prelude

import Darcs.Patch.Commute
  ( Commute, commute, selfCommuter
  )
import Darcs.Patch.CommuteFn
  ( commuterIdFL, commuterFLId
  )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.FromPrim ( PrimOf )
import Darcs.Patch.Invert
  ( Invert(..), invertFL, invertRL
  )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Viewing ()
import Darcs.Patch.Witnesses.Eq ( EqCheck(..), Eq2(..) )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered
  ( FL(..), (:>)(..), (+>+), reverseFL
  , RL(..), (+<+), reverseRL
  )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, show2 )

import Darcs.Util.Printer ( blueText, vcat )

-- | An 'Unwound' represents a primitive patch, together with any
-- other primitives that are required to place the primitive in a
-- different context. Typically, the presence of context patches
-- indicates that the underlying primitive would be in conflict in
-- the given context.
--
-- We have the following invariants:
--  - if a context contains a patch, that context does not also
--    contain the inverse of that patch (when commuted next to each other)
--  - if either context contains a patch that commutes with the underlying
--    patch, then neither context contains the inverse of that patch
--    (when commuted next to each other)
-- Another way of putting it is that all possible pairs of patch+inverse
-- that can be reached by commutation are removed.
data Unwound prim wX wY where
  Unwound
    :: FL prim wA wB        -- ^context before
    -> FL prim wB wC        -- ^underlying primitives
    -> RL prim wC wD        -- ^context after
    -> Unwound prim wA wD

deriving instance Show2 prim => Show (Unwound prim wX wY)
instance Show2 prim => Show1 (Unwound prim wX)
instance Show2 prim => Show2 (Unwound prim)

instance (PatchListFormat prim, ShowPatchBasic prim)
  => ShowPatchBasic (Unwound prim) where
  showPatch f (Unwound before prims after) =
    vcat [
      blueText "before:",
      showPatch f before,
      blueText "prims:",
      showPatch f prims,
      blueText "after:",
      showPatch f after
    ]

instance Invert prim => Invert (Unwound prim) where
  invert (Unwound before prim after)
    = Unwound (invertRL after) (invert prim) (invertFL before)

class Unwind p where
  -- | Get hold of the underlying primitives for a given patch, placed in
  -- the context of the patch. If there are conflicts then context patches
  -- will be needed.
  fullUnwind :: p wX wY -> Unwound (PrimOf p) wX wY

mkUnwound
  :: (Commute prim, Invert prim, Eq2 prim)
  => FL prim wA wB
  -> FL prim wB wC
  -> FL prim wC wD
  -> Unwound prim wA wD
mkUnwound before ps after =
  consBefores before .
  flip consAfters after $
  Unwound NilFL ps NilRL

consBefores
  :: (Commute prim, Invert prim, Eq2 prim)
  => FL prim wA wB
  -> Unwound prim wB wC
  -> Unwound prim wA wC
consBefores NilFL u = u
consBefores (b :>: bs) u = consBefore b (consBefores bs u)

consAfters
  :: (Commute prim, Invert prim, Eq2 prim)
  => Unwound prim wA wB
  -> FL prim wB wC
  -> Unwound prim wA wC
consAfters u NilFL = u
consAfters u (a :>: as) = consAfters (consAfter u a) as

consBefore
  :: (Commute prim, Invert prim, Eq2 prim)
  => prim wA wB
  -> Unwound prim wB wC
  -> Unwound prim wA wC
consBefore b (Unwound NilFL ps after) =
  case commuterIdFL selfCommuter (b :> ps) of
    Nothing -> Unwound (b :>: NilFL) ps after
    -- It is possible for a context patch to commute with the
    -- underlying primitive. If that happens we want to see if we can eliminate it
    -- by propagating it through the other context ("after" in this case).
    -- "full unwind example 3" fails if this case is omitted, as (typically) do the standard
    -- 100 iteration QuickCheck tests
    Just (ps' :> b') -> Unwound NilFL ps' (propagateAfter (NilRL :> b' :> reverseRL after))
consBefore b1 (Unwound (b2 :>: bs) ps after)
  | IsEq <- invert b1 =\/= b2 = Unwound bs ps after
  | Just (b2' :> b1') <- commute (b1 :> b2)
     = case consBefore b1' (Unwound bs ps after) of
         Unwound bs' ps' after' -> Unwound (b2' :>: bs') ps' after'
consBefore b (Unwound bs ps after) = Unwound (b :>: bs) ps after

consAfter
  :: (Commute prim, Invert prim, Eq2 prim)
  => Unwound prim wA wB
  -> prim wB wC
  -> Unwound prim wA wC
consAfter (Unwound before ps NilRL) a =
  case commuterFLId selfCommuter (ps :> a) of
    Nothing -> Unwound before ps (NilRL :<: a)
    -- as with consBefore, we need to see if we can eliminate a context patch
    -- that commutes with the underlying primitive, by propagating it through the
    -- "before" context
    -- "full unwind example 3" fails if this case is omitted, as (typically) do the standard
    -- 100 iteration QuickCheck tests
    Just (a' :> ps') -> Unwound (propagateBefore (reverseFL before :> a' :> NilFL)) ps' NilRL
consAfter (Unwound before ps (as :<: a1)) a2
  | IsEq <- invert a1 =\/= a2 = Unwound before ps as
  | Just (a2' :> a1') <- commute (a1 :> a2)
      = case consAfter (Unwound before ps as) a2' of
          Unwound before' ps' as' -> Unwound before' ps' (as' :<: a1')
consAfter (Unwound before ps as) a = Unwound before ps (as :<: a)

propagateBefore
  :: (Commute prim, Invert prim, Eq2 prim)
  => (RL prim :> prim :> FL prim) wA wB
  -> FL prim wA wB
propagateBefore (NilRL :> p :> acc) = p :>: acc
propagateBefore (qs :<: q :> p :> acc)
  | IsEq <- invert q =\/= p = reverseRL qs +>+ acc
  | Just (p' :> q') <- commute (q :> p)
      = propagateBefore (qs :> p' :> q' :>: acc)
  | otherwise = reverseRL qs +>+ q :>: p :>: acc

propagateAfter
  :: (Commute prim, Invert prim, Eq2 prim)
  => (RL prim :> prim :> FL prim) wA wB
  -> RL prim wA wB
propagateAfter (acc :> p :> NilFL) = acc :<: p
propagateAfter (acc :> p :> q :>: qs)
  | IsEq <- invert p =\/= q = acc +<+ reverseFL qs
  | Just (q' :> p') <- commute (p :> q)
      = propagateAfter (acc :<: q' :> p' :> qs)
  | otherwise = acc :<: p :<: q +<+ reverseFL qs


-- | Given a list of unwound patches, use commutation and cancellation of
-- inverses to remove intermediate contexts. This is not guaranteed to be
-- possible in general, but should be possible if the patches that were
-- unwound were all originally recorded (unconflicted) in the same context,
-- e.g. as part of the same 'Darcs.Patch.Named.Named'.
squashUnwound
  :: (Show2 prim, Commute prim, Eq2 prim, Invert prim)
  => FL (Unwound prim) wX wY
  -> Unwound prim wX wY
squashUnwound NilFL = Unwound NilFL NilFL NilRL
squashUnwound (u :>: us) =
  -- As described in consBefore/consAfter, it's possible for some of the elements
  -- in a context to commute with the underlying prim that context is attached to,
  -- so consBefore/consAfter try to cancel them by propagating through the other
  -- context.
  -- Sometimes they also won't cancel or commute with patches in the other context
  -- so when squashing we need to move them out of the way of the patches that really
  -- need to be squashed first.
  -- The unit test "full unwind example 3" fails if we remove the moveCommuting calls,
  -- as do QuickCheck tests with a lot of iterations (e.g. 100K)
  squashPair (moveCommutingToBefore u :> moveCommutingToAfter (squashUnwound us))

moveCommutingToBefore
  :: (Commute prim, Invert prim, Eq2 prim)
  => Unwound prim wA wB
  -> Unwound prim wA wB
moveCommutingToBefore (Unwound before ps after) =
  flip consAfters (reverseRL after) $
  Unwound before ps NilRL

moveCommutingToAfter
  :: (Commute prim, Invert prim, Eq2 prim)
  => Unwound prim wA wB
  -> Unwound prim wA wB
moveCommutingToAfter (Unwound before ps after) =
  consBefores before $
  Unwound NilFL ps after

squashPair
  :: (Show2 prim, Commute prim, Eq2 prim, Invert prim)
  => (Unwound prim :> Unwound prim) wX wY
  -> Unwound prim wX wY
squashPair (Unwound before ps1 NilRL :> Unwound NilFL ps2 after) =
  Unwound before (ps1 +>+ ps2) after
squashPair (Unwound before1 ps1 (after1 :<: a) :> Unwound before2 ps2 after2) =
  case pushPastForward (a :> before2) of
    before2' :> Nothing2 ->
      squashPair (Unwound before1 ps1 after1 :> Unwound before2' ps2 after2)
    before2' :> Just2 a' ->
      case commuterIdFL selfCommuter (a' :> ps2) of
        Nothing -> error $ "stuck patch: squashPair 1:\n" ++ show2 a' ++ "\n" ++ show2 ps2
        Just (ps2' :> a'') ->
          squashPair (Unwound before1 ps1 after1 :> Unwound before2' ps2' (NilRL :<: a'' +<+ after2))
squashPair (Unwound before1 ps1 NilRL :> Unwound (b :>: before2) ps2 after2) =
  case commuterFLId selfCommuter (ps1 :> b) of
    Nothing -> error "stuck patch: squashPair 2"
    Just (b' :> ps1') -> squashPair (Unwound (before1 +>+ b' :>: NilFL) ps1' NilRL :> Unwound before2 ps2 after2)

pushPastForward
  :: (Show2 prim, Commute prim, Eq2 prim, Invert prim)
  => (prim :> FL prim) wX wY
  -> (FL prim :> Maybe2 prim) wX wY
pushPastForward (p :> NilFL) = NilFL :> Just2 p
pushPastForward (p :> (q :>: qs))
  | IsEq <- invert p =\/= q = qs :> Nothing2
  | Just (q' :> p') <- commute (p :> q)
      = case pushPastForward (p' :> qs) of
          qs' :> p'' -> (q' :>: qs') :> p''
  | otherwise = error $ "stuck patch: pushPastForward:\n" ++ show2 p ++ "\n" ++ show2 q
