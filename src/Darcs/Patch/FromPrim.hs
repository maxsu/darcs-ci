module Darcs.Patch.FromPrim
    ( PrimPatchBase(..)
    , FromPrim(..)
    , ToPrim(..)
    , ToFromPrim
    ) where

import Darcs.Prelude

import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Witnesses.Ordered ( FL, RL, mapFL_FL )
import Darcs.Patch.Ident ( PatchId )
import Darcs.Patch.Info ( PatchInfo )

class PrimPatch (PrimOf p) => PrimPatchBase p where
    type PrimOf (p :: (* -> * -> *)) :: (* -> * -> *)

instance PrimPatchBase p => PrimPatchBase (FL p) where
    type PrimOf (FL p) = PrimOf p

instance PrimPatchBase p => PrimPatchBase (RL p) where
    type PrimOf (RL p) = PrimOf p

class FromPrim p where
    fromAnonymousPrim :: PrimOf p wX wY -> p wX wY
    fromPrim :: PatchId p -> PrimOf p wX wY -> p wX wY
    fromPrims :: PatchInfo -> FL (PrimOf p) wX wY -> FL p wX wY

    default fromPrim :: (PatchId p ~ ()) => PatchId p -> PrimOf p wX wY -> p wX wY
    fromPrim () = fromAnonymousPrim

    default fromPrims :: (PatchId p ~ ()) => PatchInfo -> FL (PrimOf p) wX wY -> FL p wX wY
    fromPrims _ = mapFL_FL (fromPrim ())

class ToPrim p where
    toPrim :: p wX wY -> Maybe (PrimOf p wX wY)

type ToFromPrim p = (FromPrim p, ToPrim p)
