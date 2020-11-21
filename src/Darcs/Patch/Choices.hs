-- Copyright (C) 2002-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

-- | The purpose of this module is to deal with many of the common
-- cases that come up when choosing a subset of a group of patches.
--
-- The idea is to divide a sequence of candidate patches into an initial
-- section named 'InFirst', a final section named 'InLast', and between them a
-- third section of not yet decided patches named 'InMiddle'. The reason for the
-- neutral terminology 'InFirst', 'InMiddle', and 'InLast', is that which of 'InFirst'
-- and 'InLast' counts as @selected@ or @deselected@ depends on
-- what we want to achive, that is, on the command and its options.
-- See "Darcs.UI.SelectChanges" for examples of how to use the functions from
-- this module.
--
-- Obviously if there are dependencies between the patches that will put a
-- constraint on how you can choose to divide them up. Unless stated otherwise,
-- functions that move patches from one section to another pull all dependent
-- patches with them.
--
-- Internally, we don't necessarily reorder patches immediately, but merely
-- tag them with the desired status, and thus postpone the actual commutation.
-- This saves a lot of unnecessary work, especially when choices are made
-- interactively, where the user can revise earlier decisions.
module Darcs.Patch.Choices
    ( -- * Choosing patches
      PatchChoices
    , Slot(..)
      -- ** Constructing
    , patchChoices
    , mkPatchChoices
      -- ** Querying
    , patchSlot
    , getChoices
    , separateFirstMiddleFromLast
    , separateFirstFromMiddleLast
      -- ** Forcing patches into a given 'Slot'
    , forceMatchingFirst
    , forceFirsts
    , forceFirst
    , forceMatchingLast
    , forceLasts
    , forceLast
    , forceMiddle
    , makeEverythingSooner
    , makeEverythingLater
      -- ** Operations on 'InMiddle' patches
    , selectAllMiddles
    , refineChoices
      -- ** Substitution
    , substitute
      -- * Labelling patches
    , LabelledPatch
    , Label
    , label
    , unLabel
    , labelPatches
    , getLabelInt
    ) where

import Darcs.Prelude

import Darcs.Patch.Invert ( Invert, invert )
import Darcs.Patch.Commute ( Commute, commute, commuteRL )
import Darcs.Patch.Inspect ( PatchInspect, listTouchedFiles, hunkMatches )
import Darcs.Patch.Permutations ( commuteWhatWeCanRL, commuteWhatWeCanFL )
import Darcs.Patch.Witnesses.Eq ( EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..)
    , (:>)(..), (:||:)(..)
    , zipWithFL, mapFL_FL, concatFL
    , (+>+), reverseRL, anyFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )

-- | 'Label' @mp i@ acts as a temporary identifier to help us keep track of patches
--   during the selection process.  These are useful for finding patches that
--   may have moved around during patch selection (being pushed forwards or
--   backwards as dependencies arise).
--
--   The identifier is implemented as a tuple @Label mp i@. The @i@ is an
--   integer, expected to be unique within the patches being
--   scrutinised.  The @mp@ is motivated by patch splitting; it
--   provides a convenient way to generate a new identifier from the patch
--   being split.  For example, if we split a patch identified as @Label Nothing
--   5@, the resulting sub-patches could be identified as
--   @Label (Just (Label Nothing 5))1@, @Label (Just (Label Nothing 5)) 2@, etc.
--
--   IOW, 'Label' is a non-empty, reversed list of 'Int's.
data Label = Label (Maybe Label) Int deriving Eq

-- | A patch with a 'Label' attached to it.
data LabelledPatch p wX wY = LP Label (p wX wY)

-- | This internal type tags a 'LabelledPatch' with a 'Bool', to distinguish
-- 'InMiddle' from 'InLast' patches.
data PatchChoice p wX wY = PC
  { pcPatch :: (LabelledPatch p wX wY) -- ^ the 'LabelledPatch' in question
  , _pcIsLast :: Bool                  -- ^ 'False' = 'InMiddle', 'True' = 'InLast'
  }

-- | Internal function to tag a 'LabelledPatch' as 'InMiddle' or 'InLast'.
pcSetLast :: Bool -> LabelledPatch p wX wY -> PatchChoice p wX wY
pcSetLast = flip PC

-- TODO pcsFirsts should be an 'RL', not an 'FL'.
-- | A sequence of 'LabelledPatch'es where each patch is either
-- 'InFirst', 'InMiddle', or 'InLast'. The representation is
-- optimized for the case where we start chosing patches from the left
-- of the sequence: patches that are 'InFirst' are commuted to the head
-- immediately, but patches that are 'InMiddle' or 'InLast' are mixed
-- together; when a patch is marked 'InLast', its dependencies are
-- not updated until we retrieve the final result.
data PatchChoices p wX wY where
  PCs :: { pcsFirsts :: FL (LabelledPatch p) wX wM
         , pcsMiddleLasts :: FL (PatchChoice p) wM wY}
      -> PatchChoices p wX wY

-- | See module documentation for "Darcs.Patch.Choices".
data Slot = InFirst | InMiddle | InLast

label :: LabelledPatch p wX wY -> Label
label (LP tg _) = tg

getLabelInt :: Label -> Int
getLabelInt (Label _ i) = i

unLabel :: LabelledPatch p wX wY -> p wX wY
unLabel (LP _ p) = p

-- This is dangerous if two patches from different labelled series are compared
-- ideally Label (and hence LabelledPatch/PatchChoices) would have a witness type
-- to represent the originally labelled sequence.
compareLabels :: LabelledPatch p wA wB -> LabelledPatch p wC wD -> EqCheck (wA, wB) (wC, wD)
compareLabels (LP l1 _) (LP l2 _) = if l1 == l2 then unsafeCoerceP IsEq else NotEq

instance Invert p => Invert (LabelledPatch p) where
  invert (LP t p) = LP t (invert p)

instance Commute p => Commute (LabelledPatch p) where
  commute (LP l1 p1 :> LP l2 p2) = do
    p2' :> p1' <- commute (p1 :> p2)
    return (LP l2 p2' :> LP l1 p1')

instance PatchInspect p => PatchInspect (LabelledPatch p) where
  listTouchedFiles = listTouchedFiles . unLabel
  hunkMatches f = hunkMatches f . unLabel

instance Commute p => Commute (PatchChoice p) where
  commute (PC p1 c1 :> PC p2 c2) = do
    p2' :> p1' <- commute (p1 :> p2)
    return (PC p2' c2 :> PC p1' c1)

instance PatchInspect p => PatchInspect (PatchChoice p) where
  listTouchedFiles = listTouchedFiles . pcPatch
  hunkMatches f = hunkMatches f . pcPatch

-- | Create a 'PatchChoices' from a sequence of patches, so that
-- all patches are initially 'InMiddle'.
patchChoices :: FL p wX wY -> PatchChoices p wX wY
patchChoices = mkPatchChoices . labelPatches Nothing

-- | Label a sequence of patches, maybe using the given parent label.
labelPatches :: Maybe Label -> FL p wX wY -> FL (LabelledPatch p) wX wY
labelPatches tg ps = zipWithFL LP (map (Label tg) [1..]) ps

-- | Create a 'PatchChoices' from an already labelled sequence of patches,
-- so that all patches are initially 'InMiddle'.
mkPatchChoices :: FL (LabelledPatch p) wX wY -> PatchChoices p wX wY
mkPatchChoices = PCs NilFL . mapFL_FL (pcSetLast False)


-- | Like 'getChoices' but lumps together 'InMiddle' and 'InLast' patches.
-- This is more efficient than using 'getChoices' and then catenating 'InMiddle'
-- and 'InLast' sections because we have to commute less.
-- (This is what 'PatchChoices' are optimized for.)
--
-- prop> separateFirstFromMiddleLast c == case getChoices c of f:>m:>l -> f:>m+>+l
separateFirstFromMiddleLast :: PatchChoices p wX wZ
                            -> (FL (LabelledPatch p) :> FL (LabelledPatch p)) wX wZ
separateFirstFromMiddleLast (PCs f ml) = f :> mapFL_FL pcPatch ml

-- | Like 'getChoices' but lumps together 'InFirst' and 'InMiddle' patches.
--
-- prop> separateFirstMiddleFromLast c == case getChoices c of f:>m:>l -> f+>+m:>l
separateFirstMiddleFromLast :: Commute p
                            => PatchChoices p wX wZ
                            -> (FL (LabelledPatch p) :> FL (LabelledPatch p)) wX wZ
separateFirstMiddleFromLast (PCs f l) =
  case pushLasts l of
    (m :> l') -> f +>+ m :> l'

-- | Retrieve the resulting sections from a 'PatchChoice'.  The result is a
-- triple @first:>middle:>last@, such that all patches in @first@ are
-- 'InFirst', all patches in @middle@ are 'InMiddle', and all patches in @last@
-- are 'InLast'.
getChoices :: Commute p
           => PatchChoices p wX wY
           -> (FL (LabelledPatch p) :> FL (LabelledPatch p) :> FL (LabelledPatch p)) wX wY
getChoices (PCs f ml) =
  case pushLasts ml of
    (m :> l') -> f :> m :> l'

-- | Internal function to commute patches in the common 'pcsMiddleLasts' segment
-- so that all 'InLast' patches are behind 'InMiddle' ones. Patches 'InMiddle'
-- that depend on any 'InLast' are promoted to 'InLast'.
pushLasts :: Commute p
          => FL (PatchChoice p) wX wY
          -> (FL (LabelledPatch p) :> FL (LabelledPatch p)) wX wY
pushLasts NilFL = NilFL :> NilFL
pushLasts (PC lp False :>: pcs) =
  case pushLasts pcs of
       (m :> l) -> (lp :>: m) :> l
pushLasts (PC lp True :>: pcs) =
  case pushLasts pcs of
    (m :> l) ->
      case commuteWhatWeCanFL (lp :> m) of
        (m' :> lp' :> deps) -> m' :> (lp' :>: deps +>+ l)

-- TODO for the way we use this function it is too restrictive IMO: it does not
-- allow the user to select anything that doesn't match the pre-filters.
-- | Use the given monadic 'PatchChoices' transformer on the 'InMiddle' section
-- of a 'PatchChoices', then fold the result back into the original 'PatchChoices'.
refineChoices :: (Commute p, Monad m)
              => (forall wU wV . FL (LabelledPatch p) wU wV ->
                  PatchChoices p wU wV -> m (PatchChoices p wU wV))
              -> PatchChoices p wX wY -> m (PatchChoices p wX wY)
refineChoices act ps =
  case getChoices ps of
    (f :> m :> l) -> do
      (PCs f' l') <- act m (mkPatchChoices m)
      return . PCs (f +>+ f') $ l' +>+ mapFL_FL (pcSetLast True) l

-- | Given a 'LabelledPatch' determine to which section of the given
-- 'PatchChoices' it belongs. This is not trivial to compute, since a patch
-- tagged as 'InMiddle' may be forced to actually be 'InLast' by dependencies. We
-- return a possibly re-ordered 'PatchChoices' so as not to waste the
-- commutation effort.
patchSlot :: forall p wA wB wX wY. Commute p
          => LabelledPatch p wA wB
          -> PatchChoices p wX wY
          -> (Slot, PatchChoices p wX wY)
patchSlot (LP t _) pc@(PCs f ml)
  | foundIn f = (InFirst, pc)
  | otherwise = psLast f NilRL NilRL ml
  where
    foundIn = anyFL ((== t) . label)
    psLast :: forall wM wC wL .
             FL (LabelledPatch p) wX wM ->
             RL (LabelledPatch p) wM wC ->
             RL (LabelledPatch p) wC wL ->
             FL (PatchChoice p) wL wY ->
             (Slot, PatchChoices p wX wY)
    psLast firsts middles bubble (PC lp True :>: ls)
      | label lp == t = (InLast
                      , PCs { pcsFirsts = firsts
                            , pcsMiddleLasts = settleM middles
                                         +>+ settleB bubble
                                         +>+ PC lp True :>: ls})
    psLast firsts middles bubble (PC lp False :>: ls)
      | label lp == t =
        case commuteRL (bubble :> lp) of
        Just (lp' :> bubble') -> (InMiddle,
                                 PCs { pcsFirsts = firsts
                                     , pcsMiddleLasts = settleM middles
                                                  +>+ PC lp' False
                                                  :>: settleB bubble'
                                                  +>+ ls})
        Nothing -> (InLast,
                   PCs { pcsFirsts = firsts
                       , pcsMiddleLasts = settleM middles
                                    +>+ settleB bubble
                                    +>+ PC lp True
                                    :>: ls})
    psLast firsts middles bubble (PC lp True :>: ls) =
      psLast firsts middles (bubble :<: lp) ls
    psLast firsts middles bubble (PC lp False :>: ls) =
      case commuteRL (bubble :> lp) of
        Just (lp' :> bubble') -> psLast firsts (middles :<: lp') bubble' ls
        Nothing -> psLast firsts middles (bubble :<: lp) ls
    psLast _ _ _ NilFL = error "impossible case"
    settleM middles = mapFL_FL (\lp -> PC lp False) $ reverseRL middles
    settleB bubble = mapFL_FL (\lp -> PC lp True) $ reverseRL bubble

-- | Force all patches matching the given predicate to be 'InFirst',
-- pulling any dependencies with them. This even forces any patches
-- that were already tagged 'InLast'.
forceMatchingFirst :: forall p wA wB. Commute p
                   => ( forall wX wY . LabelledPatch p wX wY -> Bool)
                   -> PatchChoices p wA wB
                   -> PatchChoices p wA wB
forceMatchingFirst pred (PCs f0 ml) = fmfLasts f0 NilRL ml
    where
      fmfLasts :: FL (LabelledPatch p) wA wM
                 -> RL (PatchChoice p) wM wN
                 -> FL (PatchChoice p) wN wB
                 -> PatchChoices p wA wB
      fmfLasts f l1 (a :>: l2)
          | pred_pc a =
            case commuteWhatWeCanRL (l1 :> a) of
              (deps :> a' :> l1') ->
                let
                  f' = f +>+ mapFL_FL pcPatch (reverseRL deps) +>+ (pcPatch a' :>: NilFL)
                in fmfLasts f' l1' l2
      fmfLasts f l1 (a :>: l2) = fmfLasts f (l1 :<: a) l2
      fmfLasts f l1 NilFL = PCs { pcsFirsts = f
                                , pcsMiddleLasts = reverseRL l1 }
      pred_pc :: forall wX wY . PatchChoice p wX wY -> Bool
      pred_pc (PC lp _) = pred lp

-- | Force all patches labelled with one of the given labels to be 'InFirst',
-- pulling any dependencies with them. This even forces any patches
-- that were already tagged 'InLast'.
forceFirsts :: Commute p
            => [Label] -> PatchChoices p wA wB -> PatchChoices p wA wB
forceFirsts ps = forceMatchingFirst ((`elem` ps) . label)

-- | Force a single patch labelled with the given label to be 'InFirst',
-- pulling any dependencies with them. This even forces any patches
-- that were already tagged 'InLast'.
forceFirst :: Commute p
           => Label -> PatchChoices p wA wB -> PatchChoices p wA wB
forceFirst p = forceMatchingFirst ((== p) . label)
--TODO: stop after having seen the patch we want to force first

-- | Make all 'InMiddle' patches either 'InFirst' or 'InLast'. This does *not*
-- modify any patches that are already determined to be 'InLast' by
-- dependencies.
selectAllMiddles :: forall p wX wY. Commute p
                 => Bool -> PatchChoices p wX wY -> PatchChoices p wX wY
selectAllMiddles True (PCs f l) = PCs f (mapFL_FL g l)
    where g (PC lp _) = PC lp True
selectAllMiddles False (PCs f l) = samf f NilRL NilRL l
  where
    samf :: forall wM1 wM2 wM3 .
           FL (LabelledPatch p) wX wM1 ->
           RL (LabelledPatch p) wM1 wM2 ->
           RL (PatchChoice p) wM2 wM3 ->
           FL (PatchChoice p) wM3 wY ->
           PatchChoices p wX wY
    samf f1 f2 l1 (pc@(PC lp False) :>: l2) =
      case commuteRL (l1 :> pc) of
        Nothing -> samf f1 f2 (l1 :<: PC lp True) l2
        Just ((PC lp' _) :> l1') -> samf f1 (f2 :<: lp') l1' l2
    samf f1 f2 l1 (PC lp True :>: l2) = samf f1 f2 (l1 :<: PC lp True) l2
    samf f1 f2 l1 NilFL = PCs (f1 +>+ reverseRL f2) (reverseRL l1)

-- | Similar to 'forceMatchingFirst' only that patches are forced to be
-- 'InLast' regardless of their previous status.
forceMatchingLast :: Commute p => (forall wX wY . LabelledPatch p wX wY -> Bool)
                  -> PatchChoices p wA wB
                  -> PatchChoices p wA wB
forceMatchingLast pred (PCs f ml) =
  forceMatchingMiddleOrLast pred True NilRL f ml

-- | Internal function working directly on the constituent parts of a
-- 'PatchChoices' and taking an accumulating 'RL' to build up a new 'InFirst'
-- section.  It forces patches to be 'InMiddle' or 'InLast', depending
-- on the 'Bool' parameter ('True' means 'InLast', 'False' means 'InMiddle').
-- It does this regardless of the previous status of patches and also pulls
-- any dependent patches with it.
forceMatchingMiddleOrLast
  :: forall p wA wB wM1 wM2 . Commute p
  => (forall wX wY . LabelledPatch p wX wY -> Bool)
  -> Bool
  -> RL (LabelledPatch p) wA wM1  -- ^ accumulator for 'InFirst' patches
  -> FL (LabelledPatch p) wM1 wM2 -- ^ original 'InFirst' section
  -> FL (PatchChoice p) wM2 wB    -- ^ original 'InMiddle' and 'InLast' section
  -> PatchChoices p wA wB
forceMatchingMiddleOrLast pred b f1 (a :>: f2) ml
  | pred a =
    case commuteWhatWeCanFL (a :> f2) of
      (f2' :> a' :> deps) ->
        let
          ml' = mapFL_FL (pcSetLast b) (a' :>: deps) +>+ ml
        in
        forceMatchingMiddleOrLast pred b f1 f2' ml'
forceMatchingMiddleOrLast pred b f1 (a :>: f2) ml =
  forceMatchingMiddleOrLast pred b (f1 :<: a) f2 ml
forceMatchingMiddleOrLast pred b f1 NilFL ml =
  PCs { pcsFirsts = reverseRL f1
      , pcsMiddleLasts = mapFL_FL choose ml
      }
  where
    choose (PC lp c) = (PC lp (if pred lp then b else c) )

-- | Force all patches labelled with one of the given labels to be 'InLast',
-- pulling any dependencies with them. This even forces any patches
-- that were previously tagged 'InFirst'.
forceLasts :: Commute p
           => [Label] -> PatchChoices p wA wB -> PatchChoices p wA wB
forceLasts ps = forceMatchingLast ((`elem` ps) . label)

-- | Force a single patch labelled with the given label to be 'InLast',
-- pulling any dependencies with them, regardless of their previous status.
forceLast :: Commute p
          => Label -> PatchChoices p wA wB -> PatchChoices p wA wB
forceLast p = forceMatchingLast ((== p) . label)

-- | Force a patch with the given 'Label' to be 'InMiddle',
-- pulling any dependencies with it, regardless of their previous status.
forceMiddle :: Commute p => Label -> PatchChoices p wA wB -> PatchChoices p wA wB
forceMiddle t (PCs f l) = forceMatchingMiddleOrLast ((== t) . label) False NilRL f l

-- | Turn 'InFirst' patches into 'InMiddle' ones and 'InMiddle' into 'InLast' ones.
makeEverythingLater :: PatchChoices p wX wY -> PatchChoices p wX wY
makeEverythingLater (PCs f ml) =
  let m = mapFL_FL (pcSetLast False) f
      ml' = mapFL_FL (\(PC lp _) -> PC lp True) ml
  in PCs NilFL $ m +>+ ml'

-- | Turn 'InMiddle' patches into 'InFirst' and 'InLast' patches into 'InMiddle'.
-- Does *not* pull dependencies into 'InFirst', instead patches that
-- cannot be commuted past 'InLast' patches stay 'InMiddle'.
makeEverythingSooner :: forall p wX wY. Commute p
                     => PatchChoices p wX wY -> PatchChoices p wX wY
makeEverythingSooner (PCs f ml) =
  case mes NilRL NilRL ml
       of (m :> ml') ->
            PCs (f +>+ m) ml'
    where
      mes :: forall wM1 wM2 wM3 .
            RL (LabelledPatch p) wM1 wM2 ->
            RL (LabelledPatch p) wM2 wM3 ->
            FL (PatchChoice p) wM3 wY ->
            (FL (LabelledPatch p) :> FL (PatchChoice p)) wM1 wY
      mes middle bubble (PC lp True :>: mls) = mes middle (bubble :<: lp) mls
      mes middle bubble (PC lp False :>: mls) =
        case commuteRL (bubble :> lp) of
          Nothing -> mes middle (bubble :<: lp) mls
          Just (lp' :> bubble') -> mes (middle :<: lp') bubble' mls
      mes middle bubble NilFL =
        (reverseRL middle) :> mapFL_FL (\lp -> PC lp False) (reverseRL bubble)

-- | Substitute a single 'LabelledPatch' with an equivalent list of patches,
-- preserving its status as 'InFirst', 'InMiddle' or 'InLast').
-- The patch is looked up using equality of 'Label's.
substitute :: forall p wX wY .
              Sealed2 (LabelledPatch p :||: FL (LabelledPatch p))
           -> PatchChoices p wX wY
           -> PatchChoices p wX wY
substitute (Sealed2 (lp :||: new_lps)) (PCs f l) =
  PCs (concatFL $ mapFL_FL substLp f) (concatFL $ mapFL_FL substPc l)
   where
     substLp :: LabelledPatch p wA wB -> FL (LabelledPatch p) wA wB
     substLp lp'
       | IsEq <- compareLabels lp lp' = new_lps
       | otherwise = lp' :>: NilFL
     substPc :: PatchChoice p wA wB -> FL (PatchChoice p) wA wB
     substPc (PC lp' c)
       | IsEq <- compareLabels lp lp' = mapFL_FL (pcSetLast c) new_lps
       | otherwise = PC lp' c :>: NilFL
