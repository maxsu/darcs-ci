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

module Darcs.Patch.TouchesFiles
    ( lookTouch
    , chooseTouching
    , deselectNotTouching
    , selectNotTouching
    ) where

import Darcs.Prelude

import Data.List ( nub )

import Darcs.Patch.Apply
       (Apply, ApplyState, applyToPaths)
import Darcs.Patch.Choices
       (PatchChoices, Label, LabelledPatch, patchChoices, label,
        getChoices, forceFirsts, forceLasts, unLabel)
import Darcs.Patch.Commute (Commute)
import Darcs.Patch.Inspect (PatchInspect)
import Darcs.Patch.Witnesses.Ordered
       (FL(..), (:>)(..), mapFL_FL, (+>+))
import Darcs.Patch.Witnesses.Sealed (Sealed, seal)

import Darcs.Util.Path (AnchoredPath, isPrefix)
import Darcs.Util.Tree (Tree)

labelTouching
  :: (Apply p, PatchInspect p, ApplyState p ~ Tree)
  => Bool -> [AnchoredPath] -> FL (LabelledPatch p) wX wY -> [Label]
labelTouching _ _ NilFL = []
labelTouching wantTouching fs (lp :>: lps) =
  case lookTouchOnlyEffect fs (unLabel lp) of
    (doesTouch, fs') ->
      let rest = labelTouching wantTouching fs' lps
      in (if doesTouch == wantTouching
            then (label lp :)
            else id)
           rest

labelNotTouchingFM
  :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
  => [AnchoredPath] -> PatchChoices p wX wY -> [Label]
labelNotTouchingFM paths pc =
  case getChoices pc of
    fc :> mc :> _ -> labelTouching False paths (fc +>+ mc)

selectTouching
  :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
  => Maybe [AnchoredPath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectTouching Nothing pc = pc
selectTouching (Just paths) pc = forceFirsts xs pc
  where
    xs =
      case getChoices pc of
        _ :> mc :> lc -> labelTouching True paths (mc +>+ lc)

deselectNotTouching
  :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
  => Maybe [AnchoredPath] -> PatchChoices p wX wY -> PatchChoices p wX wY
deselectNotTouching Nothing pc = pc
deselectNotTouching (Just paths) pc =
  forceLasts (labelNotTouchingFM paths pc) pc

selectNotTouching
  :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
  => Maybe [AnchoredPath] -> PatchChoices p wX wY -> PatchChoices p wX wY
selectNotTouching Nothing pc = pc
selectNotTouching (Just paths) pc = forceFirsts (labelNotTouchingFM paths pc) pc

chooseTouching
  :: (Apply p, Commute p, PatchInspect p, ApplyState p ~ Tree)
  => Maybe [AnchoredPath] -> FL p wX wY -> Sealed (FL p wX)
chooseTouching Nothing p = seal p
chooseTouching paths p =
  case getChoices $ selectTouching paths $ patchChoices p of
    fc :> _ :> _ -> seal $ mapFL_FL unLabel fc

lookTouchOnlyEffect
  :: (Apply p, ApplyState p ~ Tree)
  => [AnchoredPath] -> p wX wY -> (Bool, [AnchoredPath])
lookTouchOnlyEffect fs p = (wasTouched, fs')
  where
    (wasTouched, _, fs', _) = lookTouch Nothing fs p

lookTouch
  :: (Apply p, ApplyState p ~ Tree)
  => Maybe [(AnchoredPath, AnchoredPath)]
  -> [AnchoredPath]
  -> p wX wY
  -> (Bool, [AnchoredPath], [AnchoredPath], [(AnchoredPath, AnchoredPath)])
lookTouch renames fs p = (anyTouched, touchedFs, fs', renames')
  where
    touchedFs = nub . concatMap fsAffectedBy $ affected
    fsAffectedBy af = filter (affectedBy af) fs
    anyTouched = length touchedFs > 0
    affectedBy :: AnchoredPath -> AnchoredPath -> Bool
    touched `affectedBy` f =
      touched `isPrefix` f || f `isPrefix` touched
    (affected, fs', renames') = applyToPaths p renames fs
