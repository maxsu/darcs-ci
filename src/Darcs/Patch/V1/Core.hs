module Darcs.Patch.V1.Core
    ( RepoPatchV1(..),
      isMerger, mergerUndo
    ) where

import Darcs.Prelude

import Darcs.Patch.Format
    ( PatchListFormat(..)
    , ListFormat(ListFormatV1)
    )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.FromPrim
    ( FromPrim(..)
    , PrimPatchBase(..)
    , ToPrim(..)
    )
import Darcs.Patch.Ident ( PatchId )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Repair ( Check )

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, appPrec, showsPrec2 )

-- This haddock could be put on the individual bits of Merger instead
-- once haddock supports doc comments on GADT constructors
{- |
The format of a merger is @Merger undos unwindings conflicting original@.

@undos@ = the effect of the merger

@unwindings@ = TODO: eh?

@conflicting@ = the patch we conflict with

@original@ = the patch we really are
-}
data RepoPatchV1 prim wX wY where
    PP :: prim wX wY -> RepoPatchV1 prim wX wY
    Merger :: FL (RepoPatchV1 prim) wX wY
           -> RL (RepoPatchV1 prim) wX wB
           -> RepoPatchV1 prim wC wX
           -> RepoPatchV1 prim wC wD
           -> RepoPatchV1 prim wX wY
    Regrem :: FL (RepoPatchV1 prim) wX wY
           -> RL (RepoPatchV1 prim) wX wB
           -> RepoPatchV1 prim wC wX
           -> RepoPatchV1 prim wC wD
           -> RepoPatchV1 prim wY wX

instance Show2 prim => Show (RepoPatchV1 prim wX wY)  where
    showsPrec d (PP p) =
        showParen (d > appPrec) $ showString "PP " . showsPrec2 (appPrec + 1) p
    showsPrec d (Merger undos unwindings conflicting original) =
        showParen (d > appPrec) $
            showString "Merger " . showsPrec2 (appPrec + 1) undos .
            showString " " . showsPrec2 (appPrec + 1) unwindings .
            showString " " . showsPrec2 (appPrec + 1) conflicting .
            showString " " . showsPrec2 (appPrec + 1) original
    showsPrec d (Regrem undos unwindings conflicting original) =
        showParen (d > appPrec) $
            showString "Regrem " . showsPrec2 (appPrec + 1) undos .
            showString " " . showsPrec2 (appPrec + 1) unwindings .
            showString " " . showsPrec2 (appPrec + 1) conflicting .
            showString " " . showsPrec2 (appPrec + 1) original

instance Show2 prim => Show1 (RepoPatchV1 prim wX)

instance Show2 prim => Show2 (RepoPatchV1 prim)

instance PrimPatch prim => PrimPatchBase (RepoPatchV1 prim) where
    type PrimOf (RepoPatchV1 prim) = prim

type instance PatchId (RepoPatchV1 prim) = ()

instance FromPrim (RepoPatchV1 prim) where
    fromAnonymousPrim = PP

instance ToPrim (RepoPatchV1 prim) where
    toPrim (PP p) = Just p
    toPrim _ = Nothing

isMerger :: RepoPatchV1 prim wA wB -> Bool
isMerger (Merger{}) = True
isMerger (Regrem{}) = True
isMerger _ = False

mergerUndo :: RepoPatchV1 prim wX wY -> FL (RepoPatchV1 prim) wX wY
mergerUndo (Merger undo _ _ _) = undo
mergerUndo _ = error "impossible case"

instance PatchListFormat (RepoPatchV1 prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect Patch to be used with any other argument
   -- anyway, so it doesn't matter.
   patchListFormat = ListFormatV1

instance Check (RepoPatchV1 prim)
   -- no checks

instance PatchDebug prim => PatchDebug (RepoPatchV1 prim)

