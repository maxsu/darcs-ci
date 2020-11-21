{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Suspended
    ( Suspended(..)
    , countToEdit, simplifyPush, simplifyPushes
    , addFixupsToSuspended, removeFixupsFromSuspended
    , addToEditsToSuspended
    ) where

import Darcs.Prelude

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.Invert ( invert )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Info ( replaceJunk )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim(..), FromPrim(..) )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..), namedToFixups )
import Darcs.Patch.Rebase.Name ( RebaseName(..) )
import Darcs.Patch.RepoPatch ( RepoPatch )
import qualified Darcs.Patch.Rebase.Change as Change ( simplifyPush, simplifyPushes )
import Darcs.Patch.Rebase.Change ( RebaseChange(..), addNamedToRebase )
import qualified Darcs.Patch.Rebase.Legacy.Item as Item ( toRebaseChanges, RebaseItem )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Util.Parser ( lexString, lexWord )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Util.Printer ( vcat, text, blueText, ($$), (<+>) )
import qualified Darcs.Util.Diff as D ( DiffAlgorithm(MyersDiff) )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString.Char8 as BC ( pack )

-- | A single @Suspended@ patch contains the entire rebase state, in the form
-- of 'RebaseItem's.
-- 
-- The witnesses are such that a @Suspended@ appears to have no effect.
-- This behaviour is only kept so we can read old-style rebase patches,
-- where the entire rebase state was kept in a single patch on disk.
--
data Suspended p wX wY where
    Items :: FL (RebaseChange (PrimOf p)) wX wY -> Suspended p wX wX

deriving instance (Show2 p, Show2 (PrimOf p)) => Show (Suspended p wX wY)

instance (Show2 p, Show2 (PrimOf p)) => Show1 (Suspended p wX)

instance (Show2 p, Show2 (PrimOf p)) => Show2 (Suspended p)

instance (PrimPatchBase p, PatchInspect p) => PatchInspect (Suspended p) where
  listTouchedFiles (Items ps) = listTouchedFiles ps
  hunkMatches f (Items ps) = hunkMatches f ps

instance (PrimPatchBase p, PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Suspended p) where
   showPatch f (Items ps)
       = blueText "rebase" <+> text "0.2" <+> blueText "{"
         $$ vcat (mapFL (showPatch f) ps)
         $$ blueText "}"

instance (PrimPatchBase p, PatchListFormat p, ReadPatch p, RepoPatch p) => ReadPatch (Suspended p) where
   readPatch' =
    do lexString (BC.pack "rebase")
       version <- lexWord
       case () of
         _ | version == BC.pack "0.2" ->
              (lexString (BC.pack "{}") >> return (seal (Items NilFL)))
                <|>
                (unseal (Sealed . Items) <$> bracketedFL readPatch' '{' '}')
           -- version 0.1 was a very temporary intermediate state on the way to 0.2
           -- and we don't offer an upgrade path for it.
           | version == BC.pack "0.0" ->
               -- Note that if we have an "old-style" rebase, i.e. the first rebase implementation in
               -- darcs, characterised by the format string "rebase-in-progress", then only version
               -- 0.0 is possible here. On the other hand, the more recent implementation could use any
               -- version including 0.0.
               -- Unlike version 0.2, version 0.0 rebase patches on disk can contain conflicts. These are
               -- removed when reading by Item.toRebaseChanges, which ultimately calls 'fullUnwind', the
               -- same machinery that is used when version 0.2 patches are created from scratch.
               let
                 itemsToSuspended :: Sealed (FL (Item.RebaseItem p) wX) -> Sealed (Suspended p wX)
                 itemsToSuspended (Sealed ps) =
                   case Item.toRebaseChanges ps of
                     Sealed ps' -> Sealed (Items ps')
               in
               (lexString (BC.pack "{}") >> return (seal (Items NilFL)))
                <|>
                itemsToSuspended <$> bracketedFL readPatch' '{' '}'
           | otherwise -> error $ "can't handle rebase version " ++ show version

countToEdit :: Suspended p wX wY -> Int
countToEdit (Items ps) = lengthFL ps

onSuspended
  :: (forall wZ . FL (RebaseChange (PrimOf p)) wY wZ -> Sealed (FL (RebaseChange (PrimOf p)) wX))
  -> Suspended p wY wY
  -> Suspended p wX wX
onSuspended f (Items ps) = unseal Items (f ps)

-- |add fixups for the name and effect of a patch to a 'Suspended'
addFixupsToSuspended
  :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
  => Named p wX wY
  -> Suspended p wY wY
  -> Suspended p wX wX
addFixupsToSuspended p = simplifyPushes D.MyersDiff (namedToFixups p)

-- |remove fixups (actually, add their inverse) for the name and effect of a patch to a 'Suspended'
removeFixupsFromSuspended
  :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
  => Named p wX wY
  -> Suspended p wX wX
  -> Suspended p wY wY
removeFixupsFromSuspended p = simplifyPushes D.MyersDiff (invert (namedToFixups p))

-- | Add 'Named' patches for editing to a 'Suspended'. The patches to be
-- suspended are renamed by replacing the junk in their 'Patchinfo'.
--
-- The reason we rename patches immediately when suspending them is that
-- the user may pull an identical copy from a clone, Which means we have
-- the same patch name twice, once in the normal repo and once suspended.
-- Furthermore, they can again suspend that copy, leaving us with multiple
-- copies of the same patch in the rebase state. This is bad because it
-- invalidates most of the invariants for RebaseName fixups. See issue2445
-- and tests/rebase-repull.sh for examples which lead to crashes when we
-- don't do the renaming here.
addToEditsToSuspended
  :: RepoPatch p
  => D.DiffAlgorithm
  -> FL (Named p) wX wY
  -> Suspended p wY wY
  -> IO (Suspended p wX wX)
addToEditsToSuspended _ NilFL items = return items
addToEditsToSuspended da (NamedP old ds ps :>: qs) items = do
  items' <- addToEditsToSuspended da qs items
  new <- replaceJunk old
  case simplifyPush da (NameFixup (Rename new old)) items' of
    Items items'' ->
      case addNamedToRebase da (NamedP new ds ps) items'' of
        Sealed items''' -> return $ Items items'''

simplifyPush
  :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
  => D.DiffAlgorithm
  -> RebaseFixup (PrimOf p) wX wY
  -> Suspended p wY wY
  -> Suspended p wX wX
simplifyPush da fixups = onSuspended (Change.simplifyPush da fixups)

simplifyPushes
  :: (PrimPatchBase p, Commute p, FromPrim p, Effect p)
  => D.DiffAlgorithm
  -> FL (RebaseFixup (PrimOf p)) wX wY
  -> Suspended p wY wY
  -> Suspended p wX wX
simplifyPushes da fixups = onSuspended (Change.simplifyPushes da fixups)
