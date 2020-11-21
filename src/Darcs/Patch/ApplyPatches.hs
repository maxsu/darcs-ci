module Darcs.Patch.ApplyPatches
    ( applyPatches
    ) where

import Darcs.Patch.Info ( displayPatchInfo )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.MonadProgress ( MonadProgress, ProgressAction(..), runProgressActions)

import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Util.Printer ( text, ($$) )

applyPatches :: (MonadProgress m, ApplyMonad (ApplyState p) m, Apply p)
             => FL (PatchInfoAnd rt p) wX wY -> m ()
applyPatches ps = runProgressActions "Applying patch" (mapFL doApply ps)
  where
    doApply hp = ProgressAction { paAction = apply (hopefully hp)
                                , paMessage = displayPatchInfo (info hp)
                                , paOnError = text "Unapplicable patch:" $$
                                              displayPatchInfo (info hp)
                                }
