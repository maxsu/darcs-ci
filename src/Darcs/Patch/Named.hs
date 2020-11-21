--  Copyright (C) 2002-2003 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Patch.Named
    ( Named(..)
    , infopatch
    , adddeps
    , anonymous
    , HasDeps(..)
    , patch2patchinfo
    , patchname
    , patchcontents
    , fmapNamed
    , fmapFL_Named
    , mergerIdNamed
    , ShowDepsFormat(..)
    , showDependencies
    ) where

import Darcs.Prelude

import Data.List.Ordered ( nubSort )
import qualified Data.Set as S

import Darcs.Patch.CommuteFn ( MergeFn, commuterIdFL, mergerIdFL )
import Darcs.Patch.Conflict ( Conflict(..) )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo, patchinfo,
                          piName, displayPatchInfo, makePatchname )
import Darcs.Patch.Merge ( CleanMerge(..), Merge(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Ident ( Ident(..), PatchId, IdEq2(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..), FromPrim(..) )
import Darcs.Util.Parser ( Parser, option, lexChar,
                                choice, skipWhile, anyChar )
import Darcs.Patch.Repair ( mapMaybeSnd, Repair(..), RepairToFL, Check(..) )
import Darcs.Patch.Show
    ( ShowContextPatch(..)
    , ShowPatch(..)
    , ShowPatchBasic(..)
    , ShowPatchFor(..)
    , displayPatch
    )
import Darcs.Patch.Summary
    ( Summary(..)
    , plainSummaryFL
    )
import Darcs.Patch.Unwind ( Unwind(..), squashUnwound )
import Darcs.Patch.Viewing () -- for ShowPatch FL instances

import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..), (:\/:)(..), (:/\:)(..)
    , FL(..), RL(..), mapFL, mapFL_FL, mapRL_RL
    , (+>+), concatRLFL, reverseFL
    , (+<<+), (+>>+), concatFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )

import Darcs.Util.IsoDate ( showIsoDateTime, theBeginning )
import Darcs.Util.Printer
    ( Doc, ($$), (<+>), text, vcat, cyanText, blueText )

-- | The @Named@ type adds a patch info about a patch, that is a name.
data Named p wX wY where
    NamedP :: !PatchInfo
           -> ![PatchInfo]
           -> !(FL p wX wY)
           -> Named p wX wY
   deriving Show
-- ^ @NamedP info deps p@ represents patch @p@ with name
-- @info@. @deps@ is a list of dependencies added at the named patch
-- level, compared with the unnamed level (ie, dependencies added with
-- @darcs record --ask-deps@).

instance PrimPatchBase p => PrimPatchBase (Named p) where
    type PrimOf (Named p) = PrimOf p

instance Effect p => Effect (Named p) where
    effect (NamedP _ _ p) = effect p

type instance PatchId (Named p) = PatchInfo

instance Ident (Named p) where
    ident = patch2patchinfo

instance IdEq2 (Named p)

instance IsHunk (Named p) where
    isHunk _ = Nothing

instance PatchListFormat (Named p)

instance (ReadPatch p, PatchListFormat p) => ReadPatch (Named p) where
 readPatch' = readNamed

readNamed :: (ReadPatch p, PatchListFormat p) => Parser (Sealed (Named p wX))
readNamed = do n <- readPatchInfo
               d <- readDepends
               p <- readPatch'
               return $ (NamedP n d) `mapSeal` p

readDepends :: Parser [PatchInfo]
readDepends =
  option [] $ do lexChar '<'
                 readPis

readPis :: Parser [PatchInfo]
readPis = choice [ do pi <- readPatchInfo
                      pis <- readPis
                      return (pi:pis)
                 , do skipWhile (/= '>')
                      _ <- anyChar
                      return [] ]

instance Apply p => Apply (Named p) where
    type ApplyState (Named p) = ApplyState p
    apply (NamedP _ _ p) = apply p
    unapply (NamedP _ _ p) = unapply p

instance RepairToFL p => Repair (Named p) where
    applyAndTryToFix (NamedP n d p) = mapMaybeSnd (NamedP n d) `fmap` applyAndTryToFix p

anonymous :: FromPrim p => FL (PrimOf p) wX wY -> IO (Named p wX wY)
anonymous ps = do
  info <- patchinfo (showIsoDateTime theBeginning) "anonymous" "unknown" ["anonymous"]
  return $ infopatch info ps

infopatch :: forall p wX wY. FromPrim p => PatchInfo -> FL (PrimOf p) wX wY -> Named p wX wY
infopatch pi ps = NamedP pi [] (fromPrims pi ps) where

adddeps :: Named p wX wY -> [PatchInfo] -> Named p wX wY
adddeps (NamedP pi _ p) ds = NamedP pi ds p

-- | This slightly ad-hoc class is here so we can call 'getdeps' with patch
-- types that wrap a 'Named', such as 'RebaseChange'.
class HasDeps p where
  getdeps :: p wX wY -> [PatchInfo]

instance HasDeps (Named p) where
  getdeps (NamedP _ ds _) = ds

patch2patchinfo :: Named p wX wY -> PatchInfo
patch2patchinfo (NamedP i _ _) = i

patchname :: Named p wX wY -> String
patchname (NamedP i _ _) = show $ makePatchname i

patchcontents :: Named p wX wY -> FL p wX wY
patchcontents (NamedP _ _ p) = p

patchcontentsRL :: RL (Named p) wX wY -> RL p wX wY
patchcontentsRL = concatRLFL . mapRL_RL patchcontents

fmapNamed :: (forall wA wB . p wA wB -> q wA wB) -> Named p wX wY -> Named q wX wY
fmapNamed f (NamedP i deps p) = NamedP i deps (mapFL_FL f p)

fmapFL_Named :: (FL p wA wB -> FL q wC wD) -> Named p wA wB -> Named q wC wD
fmapFL_Named f (NamedP i deps p) = NamedP i deps (f p)

instance Eq2 (Named p) where
    unsafeCompare (NamedP n1 _ _) (NamedP n2 _ _) = n1 == n2

instance Commute p => Commute (Named p) where
    commute (NamedP n1 d1 p1 :> NamedP n2 d2 p2) =
        if n2 `elem` d1 || n1 `elem` d2
        then Nothing
        else do (p2' :> p1') <- commute (p1 :> p2)
                return (NamedP n2 d2 p2' :> NamedP n1 d1 p1')

instance CleanMerge p => CleanMerge (Named p) where
    cleanMerge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
      | n1 == n2 = error "cannot cleanMerge identical Named patches"
      | otherwise = do
          p2' :/\: p1' <- cleanMerge (p1 :\/: p2)
          return $ NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

instance Merge p => Merge (Named p) where
    merge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
      | n1 == n2 = error "cannot merge identical Named patches"
      | otherwise =
          case merge (p1 :\/: p2) of
            (p2' :/\: p1') -> NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

-- Merge an unnamed patch with a named patch.
-- This operation is safe even if the first patch is named, as names can
-- never conflict with each other.
-- This is in contrast with commuterIdNamed which is not safe and hence
-- is defined closer to the code that uses it.
mergerIdNamed :: MergeFn p1 p2 -> MergeFn p1 (Named p2)
mergerIdNamed merger (p1 :\/: NamedP n2 d2 p2) =
   case mergerIdFL merger (p1 :\/: p2) of
     p2' :/\: p1' -> NamedP n2 d2 p2' :/\: p1'

{- | This instance takes care of handling the interaction between conflict
resolution and explicit dependencies. By definition, a conflict counts as
resolved if another patch depends on it. This principle extends to explicit
dependencies between 'Named' patches, but not to (aggregate) implicit
dependencies.

This means we count any patch inside a 'Named' patch as resolved if some
later 'Named' patch depends on it explicitly. The patches contained inside a
'Named' patch that is not explicitly depended on must be commuted one by one
past those we know are resolved. It is important to realize that we must not
do this commutation at the 'Named' patch level but at the level below that.
-}

instance (Commute p, Conflict p) => Conflict (Named p) where
    resolveConflicts context patches =
      case separate S.empty patches NilFL NilFL of
        deps :> nondeps ->
          resolveConflicts (patchcontentsRL context +<<+ deps) (reverseFL nondeps)
      where
        -- Separate the patch contents of an 'RL' of 'Named' patches into those
        -- we regard as resolved due to explicit dependencies on the containing
        -- 'Named' patch, and any others that can be commuted past them.
        separate :: S.Set PatchInfo
                 -> RL (Named p) w1 w2
                 -> FL p w2 w3
                 -> FL p w3 w4
                 -> (FL p :> FL p) w1 w4
        separate acc_deps (ps :<: NamedP name deps contents) resolved unresolved
          | name `S.member` acc_deps =
            -- We are depended upon explicitly, so all patches in 'contents'
            -- are considered resolved.
            separate (acc_deps +| deps) ps (contents +>+ resolved) unresolved
          | otherwise =
            -- We are not explicitly depended upon, so commute as much as we
            -- can of our patch 'contents' past 'resolved', without dragging
            -- dependencies along. To use existing tools for commutation means
            -- we have to commuteWhatWeCan 'resolved' backwards through the
            -- 'contents', now /with/ dragging dependencies along.
            case genCommuteWhatWeCanRL (commuterIdFL commute)
                  (reverseFL contents :> resolved) of
              dragged :> resolved' :> more_unresolved ->
                separate (acc_deps +| deps) ps
                  (dragged +>>+ resolved') (more_unresolved +>>+ unresolved)
        separate _ NilRL resolved unresolved = resolved :> unresolved

        -- used to accumulate explicit dependencies
        some +| more = foldr S.insert some more

instance (PrimPatchBase p, Unwind p) => Unwind (Named p) where
  fullUnwind (NamedP _ _ ps) = squashUnwound (mapFL_FL fullUnwind ps)

instance PatchInspect p => PatchInspect (Named p) where
    listTouchedFiles (NamedP _ _ p) = listTouchedFiles p
    hunkMatches f (NamedP _ _ p) = hunkMatches f p

instance Summary p => Summary (Named p) where
    conflictedEffect = conflictedEffect . patchcontents

instance Check p => Check (Named p) where
    isInconsistent (NamedP _ _ p) = isInconsistent p

-- ForStorage: note the difference between use of <> when there are
-- no explicit dependencies vs. <+> when there are
showNamedPrefix :: ShowPatchFor -> PatchInfo -> [PatchInfo] -> Doc -> Doc
showNamedPrefix f@ForStorage n [] p =
    showPatchInfo f n <> p
showNamedPrefix f@ForStorage n d p =
    showPatchInfo f n
    $$ blueText "<"
    $$ vcat (map (showPatchInfo f) d)
    $$ blueText ">"
    <+> p
showNamedPrefix f@ForDisplay n [] p =
    showPatchInfo f n
    $$ p
showNamedPrefix f@ForDisplay n d p =
    showPatchInfo f n
    $$ showDependencies ShowDepsVerbose d
    $$ p

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Named p) where
    showPatch f (NamedP n d p) = showNamedPrefix f n d $ showPatch f p

instance (Apply p, IsHunk p, PatchListFormat p,
          ShowContextPatch p) => ShowContextPatch (Named p) where
    showContextPatch f (NamedP n d p) =
        showNamedPrefix f n d <$> showContextPatch f p

data ShowDepsFormat = ShowDepsVerbose | ShowDepsSummary
                        deriving (Eq)

showDependencies :: ShowDepsFormat -> [PatchInfo] -> Doc
showDependencies format deps = vcat (map showDependency deps)
  where
    showDependency d =
      mark <+>
      cyanText (show (makePatchname d)) $$ asterisk <+> text (piName d)
    mark
      | format == ShowDepsVerbose = blueText "depend"
      | otherwise = text "D"
    asterisk = text "  *"

instance (Summary p, PatchListFormat p,
          PrimPatchBase p, ShowPatch p) => ShowPatch (Named p) where
    description (NamedP n _ _) = displayPatchInfo n
    summary (NamedP _ ds ps) =
        showDependencies ShowDepsSummary ds $$ plainSummaryFL ps
    summaryFL nps =
        showDependencies ShowDepsSummary ds $$ plainSummaryFL ps
      where
        ds = nubSort $ concat $ mapFL getdeps nps
        ps = concatFL $ mapFL_FL patchcontents nps
    content (NamedP _ ds ps) =
        showDependencies ShowDepsVerbose ds $$ displayPatch ps

instance Show2 p => Show1 (Named p wX)

instance Show2 p => Show2 (Named p)

instance PatchDebug p => PatchDebug (Named p)

