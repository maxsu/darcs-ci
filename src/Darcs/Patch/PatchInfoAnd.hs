-- Copyright (C) 2006 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Patch.PatchInfoAnd
    ( Hopefully
    , PatchInfoAnd
    , PatchInfoAndG
    , WPatchInfo
    , unWPatchInfo
    , compareWPatchInfo
    , piap
    , n2pia
    , patchInfoAndPatch
    , fmapPIAP
    , fmapFLPIAP
    , conscientiously
    , hopefully
    , info
    , winfo
    , hopefullyM
    , createHashed
    , extractHash
    , actually
    , unavailable
    , patchDesc
    ) where

import Darcs.Prelude

import Control.Exception ( Exception, throw )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.Typeable ( Typeable )

import Darcs.Util.SignalHandler ( catchNonSignal )
import Darcs.Util.Printer ( Doc, ($$), renderString, text, vcat )
import Darcs.Patch.Ident ( Ident(..), PatchId, IdEq2(..) )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo, displayPatchInfo, justName )
import Darcs.Patch.Conflict ( Conflict(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Named ( Named, fmapFL_Named )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Show ( ShowPatch(..) )
import Darcs.Patch.Repair ( Repair(..), RepairToFL )
import Darcs.Patch.RepoType ( RepoType(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowContextPatch(..) )
import Darcs.Patch.Summary ( Summary )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Witnesses.Ordered
  ( (:/\:)(..)
  , (:>)(..)
  , (:\/:)(..)
  , FL
  , mapFL
  , mapRL_RL
  )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Util.Exception ( prettyException )

-- | @'Hopefully' p C@ @(x y)@ is @'Either' String (p C@ @(x y))@ in a
-- form adapted to darcs patches. The @C@ @(x y)@ represents the type
-- witness for the patch that should be there. The @Hopefully@ type
-- just tells whether we expect the patch to be hashed or not, and
-- 'SimpleHopefully' does the real work of emulating
-- 'Either'. @Hopefully sh@ represents an expected unhashed patch, and
-- @Hashed hash sh@ represents an expected hashed patch with its hash.
data Hopefully a wX wY
    = Hopefully (SimpleHopefully a wX wY)
    | Hashed String (SimpleHopefully a wX wY)
    deriving Show

-- | @SimpleHopefully@ is a variant of @Either String@ adapted for
-- type witnesses. @Actually@ is the equivalent of @Right@, while
-- @Unavailable@ is @Left@.
data SimpleHopefully a wX wY = Actually (a wX wY) | Unavailable String
    deriving Show

type PatchInfoAnd rt p = PatchInfoAndG rt (Named p)

-- | @'PatchInfoAnd' p wA wB@ represents a hope we have to get a
-- patch through its info. We're not sure we have the patch, but we
-- know its info.
data PatchInfoAndG (rt :: RepoType) p wA wB =
  PIAP !PatchInfo
       (Hopefully p wA wB)
  deriving (Show)

-- | @'WPatchInfo' wA wB@ represents the info of a patch, marked with
-- the patch's witnesses.
newtype WPatchInfo wA wB = WPatchInfo { unWPatchInfo :: PatchInfo }

-- This is actually unsafe if we ever commute patches and then compare them
-- using this function. TODO: consider adding an extra existential to WPatchInfo
-- (as with LabelledPatch in Darcs.Patch.Choices)
compareWPatchInfo :: WPatchInfo wA wB -> WPatchInfo wC wD -> EqCheck (wA, wB) (wC, wD)
compareWPatchInfo (WPatchInfo x) (WPatchInfo y) = if x == y then unsafeCoerceP IsEq else NotEq

instance Eq2 WPatchInfo where
   WPatchInfo x `unsafeCompare` WPatchInfo y = x == y

fmapH :: (a wX wY -> b wW wZ) -> Hopefully a wX wY -> Hopefully b wW wZ
fmapH f (Hopefully sh) = Hopefully (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e
fmapH f (Hashed h sh) = Hashed h (ff sh)
    where ff (Actually a) = Actually (f a)
          ff (Unavailable e) = Unavailable e

info :: PatchInfoAndG rt p wA wB -> PatchInfo
info (PIAP i _) = i

patchDesc :: forall rt p wX wY . PatchInfoAnd rt p wX wY -> String
patchDesc p = justName $ info p

winfo :: PatchInfoAnd rt p wA wB -> WPatchInfo wA wB
winfo (PIAP i _) = WPatchInfo i

-- | @'piap' i p@ creates a PatchInfoAnd containing p with info i.
piap :: PatchInfo -> p wA wB -> PatchInfoAndG rt p wA wB
piap i p = PIAP i (Hopefully $ Actually p)

-- | @n2pia@ creates a PatchInfoAnd representing a @Named@ patch.
n2pia :: (Ident p, PatchId p ~ PatchInfo) => p wX wY -> PatchInfoAndG rt p wX wY
n2pia x = ident x `piap` x

patchInfoAndPatch :: PatchInfo -> Hopefully p wA wB -> PatchInfoAndG rt p wA wB
patchInfoAndPatch =  PIAP

fmapFLPIAP :: (FL p wX wY -> FL q wX wY)
           -> PatchInfoAnd rt p wX wY -> PatchInfoAnd rt q wX wY
fmapFLPIAP f (PIAP i hp) = PIAP i (fmapH (fmapFL_Named f) hp)

fmapPIAP :: (p wX wY -> q wX wY)
           -> PatchInfoAndG rt p wX wY -> PatchInfoAndG rt q wX wY
fmapPIAP f (PIAP i hp) = PIAP i (fmapH f hp)

-- | @'hopefully' hp@ tries to get a patch from a 'PatchInfoAnd'
-- value. If it fails, it outputs an error \"failed to read patch:
-- \<description of the patch>\". We get the description of the patch
-- from the info part of 'hp'
hopefully :: PatchInfoAndG rt p wA wB -> p wA wB
hopefully = conscientiously $ \e -> text "failed to read patch:" $$ e

-- | Using a special exception type here means that is is treated as
-- regular failure, and not as a bug in Darcs.
data PatchNotAvailable = PatchNotAvailable Doc
  deriving Typeable

instance Exception PatchNotAvailable

instance Show PatchNotAvailable where
  show (PatchNotAvailable e) = renderString e

-- | @'conscientiously' er hp@ tries to extract a patch from a 'PatchInfoAnd'.
-- If it fails, it applies the error handling function @er@ to a description
-- of the patch info component of @hp@.
-- Note: this function must be lazy in its second argument, which is why we
-- use a lazy pattern match.
conscientiously :: (Doc -> Doc)
                -> PatchInfoAndG rt p wA wB -> p wA wB
conscientiously er ~(PIAP pinf hp) =
    case hopefully2either hp of
      Right p -> p
      Left e -> throw $ PatchNotAvailable $ er (displayPatchInfo pinf $$ text e)

-- | @hopefullyM@ is a version of @hopefully@ which calls @fail@ in a
-- monad instead of erroring.
hopefullyM :: PatchInfoAndG rt p wA wB -> Maybe (p wA wB)
hopefullyM (PIAP _ hp) = case hopefully2either hp of
                              Right p -> return p
                              Left _ -> Nothing

-- Any recommendations for a nice adverb to name the below?
hopefully2either :: Hopefully a wX wY -> Either String (a wX wY)
hopefully2either (Hopefully (Actually p)) = Right p
hopefully2either (Hashed _ (Actually p)) = Right p
hopefully2either (Hopefully (Unavailable e)) = Left e
hopefully2either (Hashed _ (Unavailable e)) = Left e

actually :: a wX wY -> Hopefully a wX wY
actually = Hopefully . Actually

createHashed :: String -> (String -> IO (Sealed (a wX))) -> IO (Sealed (Hopefully a wX))
createHashed h f = mapSeal (Hashed h) `fmap` unsafeInterleaveIO (f' `catchNonSignal` handler)
  where
  f' = do Sealed x <- f h
          return (Sealed (Actually x))
  handler e = return $ seal $ Unavailable $ prettyException e

extractHash :: PatchInfoAndG rt p wA wB -> Either (p wA wB) String
extractHash (PIAP _ (Hashed s _)) = Right s
extractHash hp = Left $ conscientiously (\e -> text "unable to read patch:" $$ e) hp

unavailable :: String -> Hopefully a wX wY
unavailable = Hopefully . Unavailable

-- * Instances defined only for PatchInfoAnd

instance Show2 p => Show1 (PatchInfoAnd rt p wX)

instance Show2 p => Show2 (PatchInfoAnd rt p)

instance RepairToFL p => Repair (PatchInfoAnd rt p) where
    applyAndTryToFix p = do mp' <- applyAndTryToFix $ hopefully p
                            case mp' of
                              Nothing -> return Nothing
                              Just (e,p') -> return $ Just (e, n2pia p')

-- * Instances defined for PatchInfoAndG

instance PrimPatchBase p => PrimPatchBase (PatchInfoAndG rt p) where
   type PrimOf (PatchInfoAndG rt p) = PrimOf p

-- Equality on PatchInfoAndG is solely determined by the PatchInfo
-- It is a global invariant of darcs that once a patch is recorded,
-- it should always have the same representation in the same context.
instance Eq2 (PatchInfoAndG rt p) where
    unsafeCompare (PIAP i _) (PIAP i2 _) = i == i2

type instance PatchId (PatchInfoAndG rt p) = PatchInfo

instance Ident (PatchInfoAndG rt p) where
    ident (PIAP i _) = i

instance IdEq2 (PatchInfoAndG rt p)

instance PatchListFormat (PatchInfoAndG rt p)

instance ShowPatchBasic p => ShowPatchBasic (PatchInfoAndG rt p) where
    showPatch f (PIAP n p) =
      case hopefully2either p of
        Right x -> showPatch f x
        Left _ -> showPatchInfo f n

instance ShowContextPatch p => ShowContextPatch (PatchInfoAndG rt p) where
  showContextPatch f (PIAP n p) =
    case hopefully2either p of
      Right x -> showContextPatch f x
      Left _ -> return $ showPatchInfo f n

instance (Summary p, PatchListFormat p,
          ShowPatch p) => ShowPatch (PatchInfoAndG rt p) where
    description (PIAP n _) = displayPatchInfo n
    summary (PIAP _ p) =
      case hopefully2either p of
        Right x -> summary x
        Left _ -> text $ "[patch summary is unavailable]"
    summaryFL = vcat . mapFL summary
    content (PIAP _ p) =
      case hopefully2either p of
        Right x -> content x
        Left _ -> text $ "[patch content is unavailable]"

instance (PatchId p ~ PatchInfo, Commute p) => Commute (PatchInfoAndG rt p) where
    commute (x :> y) = do y' :> x' <- commute (hopefully x :> hopefully y)
                          return $ (ident y `piap` y') :> (ident x `piap` x')

instance (PatchId p ~ PatchInfo, CleanMerge p) =>
         CleanMerge (PatchInfoAndG rt p) where
    cleanMerge (x :\/: y)
      | ident x == ident y = error "cannot cleanMerge identical PatchInfoAndG"
      | otherwise = do
          y' :/\: x' <- cleanMerge (hopefully x :\/: hopefully y)
          return $ (ident y `piap` y') :/\: (ident x `piap` x')

instance (PatchId p ~ PatchInfo, Merge p) => Merge (PatchInfoAndG rt p) where
    merge (x :\/: y)
      | ident x == ident y = error "cannot merge identical PatchInfoAndG"
      | otherwise =
          case merge (hopefully x :\/: hopefully y) of
            y' :/\: x' -> (ident y `piap` y') :/\: (ident x `piap` x')

instance PatchInspect p => PatchInspect (PatchInfoAndG rt p) where
    listTouchedFiles = listTouchedFiles . hopefully
    hunkMatches f = hunkMatches f . hopefully

instance Apply p => Apply (PatchInfoAndG rt p) where
    type ApplyState (PatchInfoAndG rt p) = ApplyState p
    apply = apply . hopefully
    unapply = unapply .hopefully

instance ( ReadPatch p, Ident p, PatchId p ~ PatchInfo
         ) => ReadPatch (PatchInfoAndG rt p) where
    readPatch' = mapSeal n2pia <$> readPatch'

instance Effect p => Effect (PatchInfoAndG rt p) where
    effect = effect . hopefully

instance IsHunk (PatchInfoAndG rt p) where
    isHunk _ = Nothing

instance PatchDebug p => PatchDebug (PatchInfoAndG rt p)

instance (Commute p, Conflict p) => Conflict (PatchInfoAnd rt p) where
    -- Note: this relies on the laziness of 'hopefully' for efficiency
    -- and correctness in the face of lazy repositories
    resolveConflicts context patches =
      resolveConflicts (mapRL_RL hopefully context) (mapRL_RL hopefully patches)
