{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.FileUUID.Details
    ()
    where

import Darcs.Patch.Prim.Class ( PrimDetails(..) )
import Darcs.Patch.Prim.FileUUID.Core ( Prim(..) )


-- TODO
instance PrimDetails Prim where
  summarizePrim _ = []
