{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Named.Wrapped
  ( WrappedNamed(..)
  , fromRebasing
  ) where

import Darcs.Prelude

import Control.Applicative ( (<|>) )
import Data.Coerce ( coerce )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo )
import Darcs.Patch.FromPrim ( FromPrim, PrimPatchBase(..) )
import Darcs.Patch.Named ( Named(..), patch2patchinfo )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Rebase.Suspended
  ( Suspended(..)
  , addFixupsToSuspended
  , removeFixupsFromSuspended
  )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.RepoType
  ( RepoType(..), IsRepoType(..), SRepoType(..)
  , RebaseType(..), SRebaseType(..)
  )
import Darcs.Patch.Show ( ShowPatchBasic(..) )

import Darcs.Patch.Witnesses.Sealed ( mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Ordered
  ( FL(..), mapFL_FL, (:>)(..)
  )

-- |A patch that lives in a repository where an old-style rebase is in
-- progress. Such a repository will consist of @Normal@ patches
-- along with exactly one @Suspended@ patch.
--
-- It is here only so that we can upgrade an old-style rebase.
--
-- @NormalP@ represents a normal patch within a respository where a
-- rebase is in progress. @NormalP p@ is given the same on-disk
-- representation as @p@, so a repository can be switched into
-- and out of rebasing mode simply by adding or removing a
-- @RebaseP@ patch and setting the appropriate format flag.
--
-- Note that the witnesses are such that the @RebaseP@
-- patch has no effect on the context of the rest of the
-- repository; in a sense the patches within it are
-- dangling off to one side from the main repository.
data WrappedNamed (rt :: RepoType) p wX wY where
  NormalP :: !(Named p wX wY) -> WrappedNamed rt p wX wY
  RebaseP
    :: (PrimPatchBase p, FromPrim p, Effect p)
    => !PatchInfo
    -> !(Suspended p wX wX)
    -> WrappedNamed ('RepoType 'IsRebase) p wX wX

deriving instance Show2 p => Show (WrappedNamed rt p wX wY)

instance Show2 p => Show1 (WrappedNamed rt p wX)

instance Show2 p => Show2 (WrappedNamed rt p)

fromRebasing :: WrappedNamed rt p wX wY -> Named p wX wY
fromRebasing (NormalP n) = n
fromRebasing (RebaseP {}) = error "internal error: found rebasing internal patch"

instance PrimPatchBase p => PrimPatchBase (WrappedNamed rt p) where
  type PrimOf (WrappedNamed rt p) = PrimOf p

type instance PatchId (WrappedNamed rt p) = PatchInfo

instance Ident (WrappedNamed rt p) where
  ident (NormalP p) = patch2patchinfo p
  ident (RebaseP name _) = name

instance PatchListFormat (WrappedNamed rt p)

instance (ShowPatchBasic p, PatchListFormat p)
  => ShowPatchBasic (WrappedNamed rt p) where

  showPatch f (NormalP n) = showPatch f n
  showPatch f (RebaseP i s) = showPatchInfo f i <> showPatch f s

-- This is a local hack to maintain backwards compatibility with
-- the on-disk format for rebases. Previously the rebase container
-- was internally represented via a 'Rebasing' type that sat *inside*
-- a 'Named', and so the rebase container patch had the structure
-- 'NamedP i [] (Suspendended s :>: NilFL)'. This structure was reflected
-- in the way it was saved on disk.
-- The easiest to read this structure is to use an intermediate type
-- that reflects the old structure.
-- TODO: switch to a more natural on-disk structure that directly
-- saves/reads 'RebaseP'.
data ReadRebasing p wX wY where
  ReadNormal    :: p wX wY -> ReadRebasing p wX wY
  ReadSuspended :: Suspended p wX wX -> ReadRebasing p wX wX

instance ( ReadPatch p, PrimPatchBase p, FromPrim p, Effect p, PatchListFormat p
         , RepoPatch p, IsRepoType rt
         ) => ReadPatch (WrappedNamed rt p) where
  readPatch' =
    case singletonRepoType :: SRepoType rt of
      SRepoType SIsRebase ->
        let wrapNamed :: Named (ReadRebasing p) wX wY -> WrappedNamed rt p wX wY
            wrapNamed (NamedP i [] (ReadSuspended s :>: NilFL))
               = RebaseP i s
            wrapNamed (NamedP i deps ps) = NormalP (NamedP i deps (mapFL_FL unRead ps))

            unRead (ReadNormal p) = p
            unRead (ReadSuspended _) = error "unexpected suspended patch"

        in fmap (mapSeal wrapNamed) readPatch'

      _ -> fmap (mapSeal NormalP) readPatch'

instance PatchListFormat p => PatchListFormat (ReadRebasing p) where
  patchListFormat = coerce (patchListFormat :: ListFormat p)

instance (ReadPatch p, PatchListFormat p, PrimPatchBase p, RepoPatch p) => ReadPatch (ReadRebasing p) where
  readPatch' =
       mapSeal toSuspended <$> readPatch'
    <|> mapSeal ReadNormal <$> readPatch'
      where -- needed to get a suitably polymorphic type
            toSuspended :: Suspended p wX wY -> ReadRebasing p wX wY
            toSuspended (Items ps) = ReadSuspended (Items ps)

instance Apply p => Apply (WrappedNamed rt p) where
  type ApplyState (WrappedNamed rt p) = ApplyState p
  apply (NormalP n) = apply n
  -- the data type definition claims that a 'RebaseP' has no effect,
  -- so make sure it really doesn't have any
  apply (RebaseP _ _) = return ()
  unapply (NormalP n) = unapply n
  unapply (RebaseP _ _) = return ()

instance Commute p => Commute (WrappedNamed rt p) where
  commute (NormalP n1 :> NormalP n2) = do
    n2' :> n1' <- commute (n1 :> n2)
    return (NormalP n2' :> NormalP n1')

  commute (RebaseP i1 s1 :> RebaseP i2 s2) =
    -- Two rebases in sequence must have the same starting context,
    -- so they should trivially commute.
    -- This case shouldn't actually happen since each repo only has
    -- a single Suspended patch.
    return (RebaseP i2 s2 :> RebaseP i1 s1)

  commute (NormalP n1 :> RebaseP i2 s2) =
    return (RebaseP i2 (addFixupsToSuspended n1 s2) :> NormalP n1)

  commute (RebaseP i1 s1 :> NormalP n2) =
    return (NormalP n2 :> RebaseP i1 (removeFixupsFromSuspended n2 s1))
