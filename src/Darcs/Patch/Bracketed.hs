module Darcs.Patch.Bracketed
    ( Bracketed(..), mapBracketed, unBracketed
    , BracketedFL, mapBracketedFLFL, unBracketedFL
    ) where

import Darcs.Prelude

import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), mapFL, mapFL_FL, concatFL )
import Darcs.Util.Printer ( vcat, blueText, ($$) )


-- |This type exists for legacy support of on-disk format patch formats.
-- It is a wrapper type that explicitly tracks the nesting of braces and parens
-- in the on-disk representation of such patches. It is used as an intermediate
-- form when reading such patches normally, and also for round-tripping such
-- patches when checking the hash in bundles.
-- It shouldn't be used for anything else.
data Bracketed p wX wY where
  Singleton :: p wX wY -> Bracketed p wX wY            -- A single patch, not wrapped in anything
  Braced :: BracketedFL p wX wY -> Bracketed p wX wY   -- A list of patches, wrapped in {}
  Parens :: BracketedFL p wX wY -> Bracketed p wX wY   -- A list of patches, wrapped in ()

type BracketedFL p wX wY = FL (Bracketed p) wX wY

unBracketed :: Bracketed p wX wY -> FL p wX wY
unBracketed (Singleton p) = p :>: NilFL
unBracketed (Braced ps) = unBracketedFL ps
unBracketed (Parens ps) = unBracketedFL ps

unBracketedFL :: BracketedFL p wX wY -> FL p wX wY
unBracketedFL = concatFL . mapFL_FL unBracketed

mapBracketed :: (forall wA wB . p wA wB -> q wA wB) -> Bracketed p wX wY -> Bracketed q wX wY
mapBracketed f (Singleton p) = Singleton (f p)
mapBracketed f (Braced ps) = Braced (mapBracketedFLFL f ps)
mapBracketed f (Parens ps) = Parens (mapBracketedFLFL f ps)

mapBracketedFLFL :: (forall wA wB . p wA wB -> q wA wB) -> BracketedFL p wX wY -> BracketedFL q wX wY
mapBracketedFLFL f = mapFL_FL (mapBracketed f)

instance PatchListFormat (Bracketed p)

instance ShowPatchBasic p => ShowPatchBasic (Bracketed p) where
    showPatch f (Singleton p) = showPatch f p
    showPatch _ (Braced NilFL) = blueText "{" $$ blueText "}"
    showPatch f (Braced ps) = blueText "{" $$ vcat (mapFL (showPatch f) ps) $$ blueText "}"
    showPatch f (Parens ps) = blueText "(" $$ vcat (mapFL (showPatch f) ps) $$ blueText ")"

-- the ReadPatch instance is defined in Darcs.Patch.Read as it is
-- used as an intermediate form during reading of lists of patches
-- that are specified as ListFormatV1 or ListFormatV2.
