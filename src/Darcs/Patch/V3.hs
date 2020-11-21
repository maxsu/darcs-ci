{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V3 ( RepoPatchV3 ) where

import Darcs.Prelude

import Darcs.Patch.Annotate ()
import Darcs.Patch.FromPrim ( FromPrim(..) )
import Darcs.Patch.Prim.Named
  ( PrimPatchId
  , anonymousNamedPrim, namedPrim, positivePrimPatchIds
  )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )
import qualified Darcs.Patch.V3.Core as Core ( RepoPatchV3(..) )
import Darcs.Patch.V3.Resolution ()

type RepoPatchV3 = Core.RepoPatchV3 PrimPatchId

-- This instance is specialised to PrimPatchId because it is dependent
-- on the relationship between PatchInfo and PrimPatchId
instance FromPrim (RepoPatchV3 prim) where
  fromAnonymousPrim = Core.Prim . anonymousNamedPrim
  fromPrim pid p = Core.Prim (namedPrim pid p)
  fromPrims = go . positivePrimPatchIds
    where
      go :: [PrimPatchId] -> FL prim wX wY -> FL (RepoPatchV3 prim) wX wY
      go _ NilFL = NilFL
      go (pid:pids) (p:>:ps) = fromPrim pid p :>: go pids ps
      go [] _ = error "positivePrimPatchIds should return an infinite list"
