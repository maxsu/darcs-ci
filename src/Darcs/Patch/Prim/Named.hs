-- -fno-cse is here because of anonymousNamedPrim - see the comments on that
{-# OPTIONS_GHC -fno-cse #-}
-- | Wrapper for prim patches to give them an identity derived from the identity
-- of the containined Named patch.
module Darcs.Patch.Prim.Named
    ( NamedPrim
    -- accessors
    , PrimPatchId
    , namedPrim
    , positivePrimPatchIds
    , anonymousNamedPrim
    -- for testing
    , unsafePrimPatchId
    , prop_primPatchIdNonZero
    ) where

import Control.Monad ( mzero )

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import qualified Data.Binary as Binary
import Crypto.Random ( getRandomBytes )
import System.IO.Unsafe ( unsafePerformIO )

import Darcs.Prelude hiding ( take )

import Darcs.Patch.Ident ( PatchId, SignedId(..), StorableId(..) )
import Darcs.Patch.Info ( PatchInfo, makePatchname )
import Darcs.Patch.Prim.WithName ( PrimWithName(..) )
import Darcs.Patch.Show ( ShowPatchFor(..) )

import Darcs.Test.TestOnly
import Darcs.Util.Hash ( SHA1, sha1Show, sha1Read )
import Darcs.Util.Parser
import Darcs.Util.Printer

-- TODO [V3INTEGRATION]:
-- Review whether we can use a PatchInfo directly here instead of a SHA1
-- Unless we can use observable sharing, this might be significantly
-- slower/less space efficient.
-- | Signed patch identity.
-- The 'SHA1' hash of the non-inverted meta data ('PatchInfo') plus an 'Int'
-- for the sequence number within the named patch, starting with 1. The 'Int'
-- gets inverted together with the patch and must never be 0 else we could not
-- distinguish between the patch and its inverse.
data PrimPatchId = PrimPatchId !Int !SHA1
  deriving (Eq, Ord, Show)

-- | This should only be used for testing, as it exposes the internal structure
-- of a 'PrimPatchId'.
unsafePrimPatchId :: TestOnly => Int -> SHA1 -> PrimPatchId
unsafePrimPatchId = PrimPatchId

prop_primPatchIdNonZero :: PrimPatchId -> Bool
prop_primPatchIdNonZero (PrimPatchId i _) = i /= 0

instance SignedId PrimPatchId where
  positiveId (PrimPatchId i _) = i > 0
  invertId (PrimPatchId i h) = PrimPatchId (- i) h

-- | Create an infinite list of positive 'PrimPatchId's.
positivePrimPatchIds :: PatchInfo -> [PrimPatchId]
positivePrimPatchIds info = map (flip PrimPatchId (makePatchname info)) [1..]

type NamedPrim = PrimWithName PrimPatchId

namedPrim :: PrimPatchId -> p wX wY -> NamedPrim p wX wY
namedPrim = PrimWithName

type instance PatchId (NamedPrim p) = PrimPatchId

-- TODO [V3INTEGRATION]:
-- It might be nice to elide the patch identifiers from the
-- on-disk format when they are the same as that of the containing patch
-- (which is the common case when there are no conflicts).
-- It's not that easy to implement as it requires refactoring to pass
-- the patch identifier downwards.
-- The sequence numbers could also be inferred from position.
instance StorableId PrimPatchId where
  readId = do
    lexString (BC.pack "hash")
    i <- int
    skipSpace
    x <- take 40
    liftMaybe $ PrimPatchId i <$> sha1Read x
   where
     liftMaybe = maybe mzero return

  showId ForStorage (PrimPatchId i h) =
    text "hash" <+> text (show i) <+> packedString (sha1Show h)
  showId ForDisplay _ = mempty

-- Because we are using unsafePerformIO, we need -fno-cse for
-- this module. We don't need -fno-full-laziness because the
-- body of the unsafePerformIO mentions 'p' so can't float outside
-- the scope of 'p'.
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Unsafe.html
{-# NOINLINE anonymousNamedPrim #-}
anonymousNamedPrim :: p wX wY -> NamedPrim p wX wY
anonymousNamedPrim p =
  unsafePerformIO $ do
    b20 <- getRandomBytes 20
    b8 <- getRandomBytes 8
    return $
      PrimWithName
        (PrimPatchId
           (abs (Binary.decode $ BL.fromStrict b8))
           (Binary.decode $ BL.fromStrict b20))
        p
