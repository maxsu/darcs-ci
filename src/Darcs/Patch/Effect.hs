{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
module Darcs.Patch.Effect ( Effect(..) ) where

import Darcs.Prelude

import Darcs.Patch.FromPrim ( PrimOf )

import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), reverseRL
    , concatFL, mapFL_FL
    )


-- | Patches whose concrete effect can be expressed as a list of
--   primitive patches.
--
--   A minimal definition would be either of @effect@ or @effectRL@.
class Effect p where
    effect :: p wX wY -> FL (PrimOf p) wX wY

instance Effect p => Effect (FL p) where
    effect = concatFL . mapFL_FL effect

instance Effect p => Effect (RL p) where
    effect = effect . reverseRL
