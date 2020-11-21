module Darcs.Patch.RepoPatch
    ( RepoPatch
    , AnnotateRP
    , Apply(..)
    , Check(..)
    , Commute(..)
    , Conflict(..)
    , Effect(..)
    , Eq2(..)
    , FromPrim(..)
    , IsHunk(..)
    , Merge(..)
    , PatchInspect(..)
    , PatchListFormat(..)
    , PrimPatchBase(..)
    , ReadPatch(..)
    , RepairToFL(..)
    , ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , Summary(..)
    , ToPrim(..)
    , Unwind(..)
    ) where

import Darcs.Patch.Annotate ( AnnotateRP )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Conflict ( Conflict(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), PrimOf(..), FromPrim(..), ToPrim(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Repair ( RepairToFL(..), Check(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), ShowContextPatch(..) )
import Darcs.Patch.Summary ( Summary(..) )
import Darcs.Patch.Unwind ( Unwind(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..) )

type RepoPatch p =
    ( AnnotateRP p
    , Apply p
    , ApplyState p ~ ApplyState (PrimOf p)
    , Check p
    , Commute p
    , Conflict p
    , Effect p
    , Eq2 p
    , FromPrim p
    , IsHunk p
    , IsHunk (PrimOf p)
    , Merge p
    , PatchInspect p
    , PatchListFormat p
    , PrimPatchBase p
    , ReadPatch p
    , RepairToFL p
    , ShowContextPatch p
    , ShowPatch p
    , Summary p
    , ToPrim p
    , Unwind p
    )
