-- Copyright (C) 2003 David Roundy
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

module Darcs.Patch.Set
    ( PatchSet(..)
    , Tagged(..)
    , SealedPatchSet
    , Origin
    , progressPatchSet
    , patchSetTags
    , emptyPatchSet
    , appendPSFL
    , patchSet2RL
    , patchSet2FL
    , inOrderTags
    , patchSetSnoc
    , patchSetSplit
    , patchSetDrop
    ) where

import Darcs.Prelude
import Data.Maybe ( catMaybes )

import Darcs.Patch.Info ( PatchInfo, piTag )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL, RL(..), (+<+), (+<<+), (:>)(..), reverseRL,
    mapRL_RL, concatRL, mapRL )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.Progress ( progress )

-- |'Origin' is a type used to represent the initial context of a repo.
data Origin

type SealedPatchSet rt p wStart = Sealed ((PatchSet rt p) wStart)

-- |The patches in a repository are stored in chunks broken up at \"clean\"
-- tags. A tag is clean if the only patches before it in the current
-- repository ordering are ones that the tag depends on (either directly
-- or indirectly). Each chunk is stored in a separate inventory file on disk.
--
-- A 'PatchSet' represents a repo's history as the list of patches since the
-- last clean tag, and then a list of patch lists each delimited by clean tags.
--
-- Because the invariants about clean tags can only be maintained if a
-- 'PatchSet' contains the whole history, the first witness is always forced
-- to be 'Origin'. The type still has two witnesses so it can easily be used
-- with combinators like ':>' and 'Fork'.
--
-- The history is lazily loaded from disk so does not normally need to be all
-- kept in memory.
data PatchSet rt p wStart wY where
    PatchSet :: RL (Tagged rt p) Origin wX -> RL (PatchInfoAnd rt p) wX wY
             -> PatchSet rt p Origin wY

deriving instance Show2 p => Show (PatchSet rt p wStart wY)

instance Show2 p => Show1 (PatchSet rt p wStart)

instance Show2 p => Show2 (PatchSet rt p)


emptyPatchSet :: PatchSet rt p Origin Origin
emptyPatchSet = PatchSet NilRL NilRL

-- |A 'Tagged' is a single chunk of a 'PatchSet'.
-- It has a 'PatchInfo' representing a clean tag,
-- the hash of the previous inventory (if it exists),
-- and the list of patches since that previous inventory.
data Tagged rt p wX wZ where
    Tagged :: PatchInfoAnd rt p wY wZ -> Maybe String
           -> RL (PatchInfoAnd rt p) wX wY -> Tagged rt p wX wZ

deriving instance Show2 p => Show (Tagged rt p wX wZ)

instance Show2 p => Show1 (Tagged rt p wX)

instance Show2 p => Show2 (Tagged rt p)


-- |'patchSet2RL' takes a 'PatchSet' and returns an equivalent, linear 'RL' of
-- patches.
patchSet2RL :: PatchSet rt p wStart wX -> RL (PatchInfoAnd rt p) wStart wX
patchSet2RL (PatchSet ts ps) = concatRL (mapRL_RL ts2rl ts) +<+ ps
  where
    ts2rl :: Tagged rt p wY wZ -> RL (PatchInfoAnd rt p) wY wZ
    ts2rl (Tagged t _ ps2) = ps2 :<: t

-- |'patchSet2FL' takes a 'PatchSet' and returns an equivalent, linear 'FL' of
-- patches.
patchSet2FL :: PatchSet rt p wStart wX -> FL (PatchInfoAnd rt p) wStart wX
patchSet2FL = reverseRL . patchSet2RL

-- |'appendPSFL' takes a 'PatchSet' and a 'FL' of patches that "follow" the
-- PatchSet, and concatenates the patches into the PatchSet.
appendPSFL :: PatchSet rt p wStart wX -> FL (PatchInfoAnd rt p) wX wY
           -> PatchSet rt p wStart wY
appendPSFL (PatchSet ts ps) newps = PatchSet ts (ps +<<+ newps)

-- |Runs a progress action for each tag and patch in a given PatchSet, using
-- the passed progress message. Does not alter the PatchSet.
progressPatchSet :: String -> PatchSet rt p wStart wX -> PatchSet rt p wStart wX
progressPatchSet k (PatchSet ts ps) =
    PatchSet (mapRL_RL progressTagged ts) (mapRL_RL prog ps)
  where
    prog = progress k

    progressTagged :: Tagged rt p wY wZ -> Tagged rt p wY wZ
    progressTagged (Tagged t h tps) = Tagged (prog t) h (mapRL_RL prog tps)

-- | The tag names of /all/ tags of a given 'PatchSet'.
patchSetTags :: PatchSet rt p wX wY -> [String]
patchSetTags = catMaybes . mapRL (piTag . info) . patchSet2RL

inOrderTags :: PatchSet rt p wS wX -> [PatchInfo]
inOrderTags (PatchSet ts _) = go ts
  where go :: RL(Tagged rt t1) wT wY -> [PatchInfo]
        go (ts' :<: Tagged t _ _) = info t : go ts'
        go NilRL = []

patchSetSnoc :: PatchSet rt p wX wY -> PatchInfoAnd rt p wY wZ -> PatchSet rt p wX wZ
patchSetSnoc (PatchSet ts ps) p = PatchSet ts (ps :<: p)

-- | Split a 'PatchSet' /before/ the latest known clean tag. The left part
-- is what comes before the tag, the right part is the tag and its
-- non-dependencies.
patchSetSplit :: PatchSet rt p wX wY
              -> (PatchSet rt p :> RL (PatchInfoAnd rt p)) wX wY
patchSetSplit (PatchSet (ts :<: Tagged t _ ps') ps) =
  PatchSet ts ps' :> ((NilRL :<: t) +<+ ps)
patchSetSplit (PatchSet NilRL ps) = PatchSet NilRL NilRL :> ps

-- | Drop the last @n@ patches from the given 'PatchSet'.
patchSetDrop :: Int
             -> PatchSet rt p wStart wX
             -> SealedPatchSet rt p wStart
patchSetDrop n ps | n <= 0 = Sealed ps
patchSetDrop n (PatchSet (ts :<: Tagged t _ ps) NilRL) =
  patchSetDrop n $ PatchSet ts (ps :<: t)
patchSetDrop _ (PatchSet NilRL NilRL) = Sealed $ PatchSet NilRL NilRL
patchSetDrop n (PatchSet ts (ps :<: _)) = patchSetDrop (n - 1) $ PatchSet ts ps
