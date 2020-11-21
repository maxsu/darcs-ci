{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Mangle () where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC (pack, last)
import qualified Data.ByteString as B (null, ByteString)
import Data.Maybe ( isJust, listToMaybe )
import Data.List ( sort, intercalate, nub )

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Inspect ( PatchInspect(listTouchedFiles) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Prim.Class
    ( PrimConstruct(primFromHunk)
    , PrimMangleUnravelled(..)
    )
import Darcs.Patch.Prim.V1.Core ( Prim )
import Darcs.Patch.Witnesses.Ordered ( FL(..), (+>+), mapFL_FL_M )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal, unseal )

import Darcs.Util.Path ( AnchoredPath )

-- | The state of a single file as far as we know it. 'Nothing'
-- means we don't know the content of a particular line.
newtype FileState wX = FileState { content :: [Maybe B.ByteString] }

-- | An infinite list of undefined lines.
unknownFileState :: FileState wX
unknownFileState = FileState (repeat Nothing)

-- | Note that @applyHunk p . applyHunk (invert p) /= id@: it converts
-- undefined lines ('Nothing') to defined ones ('Just' the old content of @p@).
applyHunk :: FileHunk wX wY -> FileState wX -> FileState wY
applyHunk (FileHunk _ line old new) = FileState . go . content
  where
    go mls =
      case splitAt (line - 1) mls of
        (before, rest) ->
          concat [before, map Just new, drop (length old) rest]

-- | Iterate 'applyHunk'.
applyHunks :: FL FileHunk wX wY -> FileState wX -> FileState wY
applyHunks NilFL = id
applyHunks (p:>:ps) = applyHunks ps . applyHunk p


instance PrimMangleUnravelled Prim where
  mangleUnravelled pss = do
      hunks <- onlyHunks pss
      filename <- listToMaybe (filenames pss)
      return $ mapSeal ((:>: NilFL) . primFromHunk) $ mangleHunks filename hunks
    where
      -- | The names of all touched files.
      filenames = nub . concatMap (unseal listTouchedFiles)

      -- | Convert every prim in the input to a 'FileHunk', or fail.
      onlyHunks :: forall prim wX. IsHunk prim
                => [Sealed (FL prim wX)]
                -> Maybe [Sealed (FL FileHunk wX)]
      onlyHunks = mapM toHunk where
        toHunk :: Sealed (FL prim wA) -> Maybe (Sealed (FL FileHunk wA))
        toHunk (Sealed ps) = fmap Sealed $ mapFL_FL_M isHunk ps

      -- | Mangle a list of hunks, returning a single hunk.
      -- Note: the input list consists of 'FL's because when commuting conflicts
      -- to the head we may accumulate dependencies. In fact, the patches in all
      -- of the given (mutually conflicting) 'FL's should coalesce to a single hunk.
      mangleHunks :: AnchoredPath -> [Sealed (FL FileHunk wX)] -> Sealed (FileHunk wX)
      mangleHunks _ [] = error "mangleHunks called with empty list of alternatives"
      mangleHunks path ps = Sealed (FileHunk path l old new)
        where
          oldf    = foldl oldFileState unknownFileState ps
          newfs   = map (newFileState oldf) ps
          l       = getHunkline (Sealed oldf : newfs)
          nchs    = sort (map (makeChunk l) newfs)
          old     = makeChunk l (Sealed oldf)
          new     = [top] ++ old ++ [initial] ++ intercalate [middle] nchs ++ [bottom]
          top     = BC.pack ("v v v v v v v" ++ eol_c)
          initial = BC.pack ("=============" ++ eol_c)
          middle  = BC.pack ("*************" ++ eol_c)
          bottom  = BC.pack ("^ ^ ^ ^ ^ ^ ^" ++ eol_c)
          -- simple heuristic to infer the line ending convention from patch contents
          eol_c   =
            if any (\line -> not (B.null line) && BC.last line == '\r') old
              then "\r"
              else ""

      -- | Apply the patches and their inverse. This turns all lines touched
      -- by the 'FL' of patches into defined lines with their "old" values.
      oldFileState :: FileState wX -> Sealed (FL FileHunk wX) -> FileState wX
      oldFileState mls (Sealed ps) = applyHunks (ps +>+ invert ps) mls

      -- | This is @flip 'applyHunks'@ under 'Sealed'.
      newFileState :: FileState wX -> Sealed (FL FileHunk wX) -> Sealed FileState
      newFileState mls (Sealed ps) = Sealed (applyHunks ps mls)

      -- Index of the first line touched by any of the FileStates (1-based).
      getHunkline :: [Sealed FileState] -> Int
      getHunkline = go 1 . map (unseal content)
        where
          -- head and tail are safe here because all inner lists are infinite
          go n pps =
            if any (isJust . head) pps
              then n
              else go (n + 1) $ map tail pps

      -- | The chunk of defined lines starting at the given position (1-based).
      makeChunk :: Int -> Sealed FileState -> [B.ByteString]
      makeChunk n = takeWhileJust . drop (n - 1) . unseal content
        where
          -- stolen from utility-ht, thanks Henning!
          takeWhileJust = foldr (\x acc -> maybe [] (:acc) x) []
