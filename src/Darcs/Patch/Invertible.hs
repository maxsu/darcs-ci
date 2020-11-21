{- | Formal inverses for patches that aren't really invertible. Note that
most the mixed {'Fwd','Rev'} cases for 'Commute' and 'Eq2' are just errors.
-}
module Darcs.Patch.Invertible
    ( Invertible
    , mkInvertible
    , fromPositiveInvertible
    , withInvertible
    ) where

import Darcs.Prelude

import Darcs.Patch.CommuteFn ( invertCommuter )
import Darcs.Patch.Ident
  ( Ident(..), PatchId, SignedId(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.RepoPatch
    ( Apply(..)
    , Commute(..)
    , Eq2(..)
    , PrimPatchBase(..)
    , PatchInspect(..)
    , ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Patch.Witnesses.Ordered ((:>)(..))

-- | Wrapper type to allow formal inversion of patches which aren't really
-- invertible.
data Invertible p wX wY where
   Fwd :: p wX wY -> Invertible p wX wY
   Rev :: p wX wY -> Invertible p wY wX

-- | Wrap a patch to make it (formally) 'Invertible'. The result is initially
-- positive i.e. 'Fwd'.
mkInvertible :: p wX wY -> Invertible p wX wY
mkInvertible = Fwd

-- | Get the underlying patch from an 'Invertible', assuming (as a precondition)
-- that it is positive i.e. 'Fwd'.
fromPositiveInvertible :: Invertible p wX wY -> p wX wY
fromPositiveInvertible (Fwd p) = p
fromPositiveInvertible (Rev _) = error "precondition of fromPositiveInvertible"

-- | Run a function on the patch inside an 'Invertible'. The function has to be
-- parametric in the witnesses, so we can run it with both a 'Fwd' and a 'Rev'
-- patch.
withInvertible :: (forall wA wB. p wA wB -> r) -> Invertible p wX wY -> r
withInvertible f (Fwd p) = f p
withInvertible f (Rev p) = f p

instance Invert (Invertible p) where
  invert (Fwd p) = Rev p
  invert (Rev p) = Fwd p

instance Commute p => Commute (Invertible p) where
  commute (Fwd p :> Fwd q) = do
    q' :> p' <- commute (p :> q)
    return (Fwd q' :> Fwd p')
  commute pair@(Rev _ :> Rev _) = invertCommuter commute pair
  commute _ = error "cannote commute mixed Fwd/Rev"

instance Eq2 p => Eq2 (Invertible p) where
  Fwd p =\/= Fwd q = p =\/= q
  Rev p =\/= Rev q = p =/\= q
  _ =\/= _ = error "cannot compare mixed Fwd/Rev"

instance Apply p => Apply (Invertible p) where
  type ApplyState (Invertible p) = ApplyState p
  apply (Fwd p) = apply p
  apply (Rev p) = unapply p
  unapply (Fwd p) = unapply p
  unapply (Rev p) = apply p

data InvertibleId ident = InvertibleId Bool ident
  deriving (Eq, Ord)

instance Ord ident => SignedId (InvertibleId ident) where
  positiveId (InvertibleId inverted _) = inverted
  invertId (InvertibleId inverted theid) =
     InvertibleId (not inverted) theid

type instance PatchId (Invertible p) = InvertibleId (PatchId p)

instance Ident p => Ident (Invertible p) where
  ident (Fwd p) = InvertibleId False (ident p)
  ident (Rev p) = InvertibleId True  (ident p)

instance PatchInspect p => PatchInspect (Invertible p) where
  listTouchedFiles (Fwd p) = listTouchedFiles p
  listTouchedFiles (Rev p) = listTouchedFiles p
  hunkMatches f (Fwd p) = hunkMatches f p
  hunkMatches f (Rev p) = hunkMatches f p

instance PrimPatchBase p => PrimPatchBase (Invertible p) where
  type PrimOf (Invertible p) = PrimOf p

instance ShowPatchBasic p => ShowPatchBasic (Invertible p) where
  showPatch ForStorage = error "Invertible patches must not be stored"
  showPatch ForDisplay = withInvertible (showPatch ForDisplay)

instance ShowPatch p => ShowPatch (Invertible p) where
  -- note these are only used for display
  description = withInvertible description
  summary = withInvertible summary
  content = withInvertible content

instance ShowContextPatch p => ShowContextPatch (Invertible p) where
  showContextPatch ForStorage = error "Invertible patches must not be stored"
  showContextPatch ForDisplay = withInvertible (showContextPatch ForDisplay)
