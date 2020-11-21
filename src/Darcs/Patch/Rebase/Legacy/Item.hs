{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Legacy.Item
    ( RebaseItem(..)
    , toRebaseChanges
    ) where

import Darcs.Prelude

import Darcs.Patch.Format ( PatchListFormat(..) )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase, PrimOf )
import Darcs.Patch.Rebase.Change ( RebaseChange(..), addNamedToRebase )
import Darcs.Patch.Rebase.Fixup ( RebaseFixup(..) )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Util.Parser ( Parser, lexString )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import qualified Darcs.Util.Diff as D

import Control.Applicative ( (<|>) )
import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

-- |A single item in the rebase state consists of either
-- a patch that is being edited, or a fixup that adjusts
-- the context so that a subsequent patch that is being edited
-- \"makes sense\".
--
-- @ToEdit@ holds a patch that is being edited. The name ('PatchInfo') of
-- the patch will typically be the name the patch had before
-- it was added to the rebase state; if it is moved back
-- into the repository it must be given a fresh name to account
-- for the fact that it will not necessarily have the same
-- dependencies or content as the original patch. This is typically
-- done by changing the @Ignore-This@ junk.
--
-- @Fixup@ adjusts the context so that a subsequent @ToEdit@ patch
-- is correct. Where possible, @Fixup@ changes are commuted
-- as far as possible into the rebase state, so any remaining
-- ones will typically cause a conflict when the @ToEdit@ patch
-- is moved back into the repository.
data RebaseItem p wX wY where
    ToEdit :: Named p wX wY -> RebaseItem p wX wY
    Fixup :: RebaseFixup (PrimOf p) wX wY -> RebaseItem p wX wY

deriving instance (Show2 p, Show2 (PrimOf p)) => Show (RebaseItem p wX wY)

instance (Show2 p, Show2 (PrimOf p)) => Show1 (RebaseItem p wX)

instance (Show2 p, Show2 (PrimOf p)) => Show2 (RebaseItem p)

toRebaseChanges
  :: forall p wX wY
   . RepoPatch p
  => FL (RebaseItem p) wX wY
  -> Sealed (FL (RebaseChange (PrimOf p)) wX)
toRebaseChanges NilFL = Sealed NilFL
toRebaseChanges (Fixup f :>: ps) =
    case toRebaseChanges ps of
      Sealed (RC fixups toedit :>: rest) -> Sealed (RC (f :>: fixups) toedit :>: rest)
      Sealed NilFL -> error "rebase chain with Fixup at end"
toRebaseChanges (ToEdit te :>: ps) =
  unseal (addNamedToRebase @p D.MyersDiff te) (toRebaseChanges ps)

-- This Read instance partly duplicates the instances for RebaseFixup,
-- but are left this way given this code is now here only for backwards compatibility of the on-disk
-- format and we might want to make future changes to RebaseFixup.
instance (PrimPatchBase p, PatchListFormat p, ReadPatch p) => ReadPatch (RebaseItem p) where
   readPatch' = mapSeal ToEdit              <$> readWith (BC.pack "rebase-toedit") <|>
                mapSeal (Fixup . PrimFixup) <$> readWith (BC.pack "rebase-fixup" ) <|>
                mapSeal (Fixup . NameFixup) <$> readWith (BC.pack "rebase-name"  )
     where readWith :: forall q wX . ReadPatch q => B.ByteString -> Parser (Sealed (q wX))
           readWith str = do lexString str
                             lexString (BC.pack "(")
                             res <- readPatch'
                             lexString (BC.pack ")")
                             return res
