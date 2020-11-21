-- it is stupid that we need UndecidableInstances just to call another
-- type function (see instance Apply below which requires this)
{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.V1.Prim ( Prim(..) ) where

import Darcs.Prelude

import Data.Coerce ( coerce )

import Darcs.Patch.Annotate ( Annotate(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format
    ( PatchListFormat(..)
    , ListFormat(ListFormatV1)
    , FileNameFormat(FileNameFormatV1,FileNameFormatDisplay) )
import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Merge ( CleanMerge )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..)
    , ShowPatchFor(..)
    , ShowPatch(..)
    , ShowContextPatch(..)
    )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )

import Darcs.Patch.Witnesses.Eq ( Eq2 )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Sealed ( mapSeal )

import Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimShow(..), PrimRead(..)
    , PrimApply(..)
    , PrimSift(..)
    , PrimMangleUnravelled(..)
    )
import qualified Darcs.Patch.Prim.V1 as Base ( Prim )

newtype Prim x y = Prim { unPrim :: Base.Prim x y } deriving
    ( Annotate
    , Apply
    , CleanMerge
    , Commute
    , Invert
    , IsHunk
    , Eq2
    , PatchInspect
    , PrimApply
    , PrimCanonize
    , PrimClassify
    , PrimConstruct
    , PrimDetails
    , PrimMangleUnravelled
    , PrimSift
    , Show
    )

instance Show1 (Prim wX)

instance Show2 Prim

instance ReadPatch Prim where
  readPatch' = fmap (mapSeal Prim) (readPrim FileNameFormatV1)

fileNameFormat :: ShowPatchFor -> FileNameFormat
fileNameFormat ForDisplay = FileNameFormatDisplay
fileNameFormat ForStorage = FileNameFormatV1

instance ShowPatchBasic Prim where
  showPatch fmt = showPrim (fileNameFormat fmt) . unPrim

instance ShowContextPatch Prim where
  showContextPatch fmt = showPrimCtx (fileNameFormat fmt) . unPrim

instance ShowPatch Prim where
  summary = plainSummaryPrim . unPrim
  summaryFL = plainSummaryPrims False
  thing _ = "change"

instance PatchListFormat Prim where
  patchListFormat = ListFormatV1

instance RepairToFL Prim where
  applyAndTryToFixFL = fmap coerce . applyAndTryToFixFL . unPrim
