module Darcs.Patch.Inspect
       ( PatchInspect(..)
       )
       where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Ordered ( FL, RL, reverseRL, mapFL )
import Darcs.Util.Path ( AnchoredPath )

import qualified Data.ByteString.Char8 as BC
import Data.List ( nub )

-- TODO Whether a patch touches a given file is not an invariant property of a
-- patch: it depends on the context i.e. it changes when we re-order patches.
-- Can we define an interface where this becomes an invariant property?

-- TODO This interface only makes sense if @ApplyState p ~ Tree@. To support
-- other ApplyStates we need to devise an abstraction for "objects" of the
-- ApplyState.

class PatchInspect p where
    listTouchedFiles :: p wX wY -> [AnchoredPath]
    hunkMatches :: (BC.ByteString -> Bool) -> p wX wY -> Bool

instance PatchInspect p => PatchInspect (FL p) where
    listTouchedFiles xs = nub $ concat $ mapFL listTouchedFiles xs
    hunkMatches f = or . mapFL (hunkMatches f)

instance PatchInspect p => PatchInspect (RL p) where
    listTouchedFiles = listTouchedFiles . reverseRL
    hunkMatches f = hunkMatches f . reverseRL

