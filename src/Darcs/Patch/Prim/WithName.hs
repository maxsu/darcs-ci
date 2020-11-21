-- | Generic wrapper for prim patches to give them an identity.
module Darcs.Patch.Prim.WithName
  ( PrimWithName(..)
  ) where

import Darcs.Prelude

import Darcs.Patch.Annotate ( Annotate(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.Ident
    ( Ident(..)
    , PatchId
    , SignedId(..)
    , StorableId(..)
    , IdEq2(..)
    )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Prim.Class ( PrimApply(..), PrimClassify(..), PrimDetails(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Merge ( CleanMerge(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..)
    , ShowPatch(..)
    , ShowContextPatch(..)
    )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered ( mapFL_FL, (:>)(..), (:\/:)(..), (:/\:)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, appPrec, showsPrec2 )

import Darcs.Util.Printer

-- |A 'PrimWithName' is a general way of associating an identity
-- with an underlying (presumably unnamed) primitive type. This is
-- required, for example, for V3 patches.
-- Normally the members of the 'name' type will be generated in
-- some way when a patch is initially created, to guarantee global
-- unqiueness across all repositories.
data PrimWithName name p wX wY =
  PrimWithName { wnName :: !name, wnPatch :: !(p wX wY) }

type instance PatchId (PrimWithName name p) = name

instance SignedId name => Ident (PrimWithName name p) where
  ident = wnName

instance (SignedId name, Eq2 p) => IdEq2 (PrimWithName name p)

instance (Eq name, Eq2 p) => Eq2 (PrimWithName name p) where
  PrimWithName i p =\/= PrimWithName j q
    | i == j, IsEq <- p =\/= q = IsEq
    | otherwise = NotEq

instance (Invert p, SignedId name) => Invert (PrimWithName name p) where
  invert (PrimWithName i p) = PrimWithName (invertId i) (invert p)

instance PatchInspect p => PatchInspect (PrimWithName name p) where
  listTouchedFiles = listTouchedFiles . wnPatch
  hunkMatches m = hunkMatches m . wnPatch

instance (Show2 p, Show name) => Show (PrimWithName name p wX wY) where
  showsPrec d (PrimWithName i p) =
    showParen (d > appPrec)
      $ showString "PrimWithName "
      . showsPrec (appPrec + 1) i
      . showString " "
      . showsPrec2 (appPrec + 1) p

instance (Show2 p, Show name) => Show1 (PrimWithName name p wX)

instance (Show2 p, Show name) => Show2 (PrimWithName name p)

instance Apply p => Apply (PrimWithName name p) where
  type ApplyState (PrimWithName name p) = ApplyState p
  apply = apply . wnPatch
  unapply = unapply . wnPatch

instance PatchListFormat (PrimWithName name p)

instance Apply p => RepairToFL (PrimWithName name p) where
  applyAndTryToFixFL p = apply p >> return Nothing

instance Annotate p => Annotate (PrimWithName name p) where
  annotate = annotate . wnPatch

instance IsHunk p => IsHunk (PrimWithName name p) where
  isHunk = isHunk . wnPatch

instance PrimApply p => PrimApply (PrimWithName name p) where
  applyPrimFL = applyPrimFL . mapFL_FL wnPatch

instance PrimClassify p => PrimClassify (PrimWithName name p) where
  primIsAddfile = primIsAddfile . wnPatch
  primIsRmfile = primIsRmfile . wnPatch
  primIsAdddir = primIsAdddir . wnPatch
  primIsRmdir = primIsRmdir . wnPatch
  primIsHunk = primIsHunk . wnPatch
  primIsMove = primIsMove . wnPatch
  primIsBinary = primIsBinary . wnPatch
  primIsTokReplace = primIsTokReplace . wnPatch
  primIsSetpref = primIsSetpref . wnPatch
  is_filepatch = is_filepatch . wnPatch

instance PrimDetails p => PrimDetails (PrimWithName name p) where
  summarizePrim = summarizePrim . wnPatch

-- this is the most important definition:
-- it ensures that a patch conflicts with itself
instance (SignedId name, Commute p) => Commute (PrimWithName name p) where
  commute (PrimWithName i1 p1 :> PrimWithName i2 p2)
    -- We should never get into a situation where we try
    -- to commute identical patches
    | i1 == i2 = error "internal error: trying to commute identical patches"
    -- whereas this case is the equivalent of merging a patch
    -- with itself, so it is correct to just report that they don't commute
    | i1 == invertId i2 = Nothing
    | otherwise = do
        p2' :> p1' <- commute (p1 :> p2)
        return (PrimWithName i2 p2' :> PrimWithName i1 p1')

instance (SignedId name, CleanMerge p) => CleanMerge (PrimWithName name p) where
  cleanMerge (PrimWithName i1 p1 :\/: PrimWithName i2 p2)
    | i1 == i2 = error "cannot cleanMerge identical patches"
    | otherwise = do
        p2' :/\: p1' <- cleanMerge (p1 :\/: p2)
        return $ PrimWithName i2 p2' :/\: PrimWithName i1 p1'

instance (StorableId name, ReadPatch p) => ReadPatch (PrimWithName name p) where
  readPatch' = do
      name <- readId
      Sealed p <- readPatch'
      return (Sealed (PrimWithName name p))

instance (StorableId name, ShowPatchBasic p) => ShowPatchBasic (PrimWithName name p) where
  showPatch use (PrimWithName name p) = showId use name $$ showPatch use p

instance (StorableId name, PrimDetails p, ShowPatchBasic p) => ShowPatch (PrimWithName name p) where
  summary = plainSummaryPrim . wnPatch
  summaryFL = plainSummaryPrims False
  thing _ = "change"

instance (StorableId name, ShowContextPatch p) => ShowContextPatch (PrimWithName name p) where
  showContextPatch use (PrimWithName name p) = do
    r <- showContextPatch use p
    return $ showId use name $$ r
