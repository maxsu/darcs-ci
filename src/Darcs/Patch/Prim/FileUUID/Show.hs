{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Darcs.Patch.Prim.FileUUID.Show
    ( displayHunk )
    where

import Darcs.Prelude

import qualified Data.ByteString as B

import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..), ShowPatch(..)
    , ShowContextPatch(..), ShowPatchFor(..) )
import Darcs.Patch.Summary ( plainSummaryPrim )
import Darcs.Patch.Prim.Class ( PrimShow(..) )
import Darcs.Patch.Prim.FileUUID.Core
    ( Prim(..), Hunk(..), HunkMove(..), UUID(..), Location(..), FileContent )
import Darcs.Patch.Prim.FileUUID.Details ()
import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Path ( Name, encodeWhiteName )
import Darcs.Util.Printer
    ( text, packedString, blueText, prefix
    , (<+>), ($$), Doc, vcat
    )

-- TODO this instance shouldn't really be necessary, as Prims aren't used generically
instance PatchListFormat Prim

fileNameFormat :: ShowPatchFor -> FileNameFormat
fileNameFormat ForDisplay = FileNameFormatDisplay
fileNameFormat ForStorage = FileNameFormatV2

instance ShowPatchBasic Prim where
  showPatch fmt = showPrim (fileNameFormat fmt)

-- dummy instance, does not actually show any context
instance ShowContextPatch Prim where
  -- showContextPatch f = showPrimCtx (fileNameFormat f)
  showContextPatch f p = return $ showPatch f p

instance ShowPatch Prim where
  summary = plainSummaryPrim
  -- summaryFL = plainSummaryPrims False
  thing _ = "change"

instance PrimShow Prim where
  showPrim FileNameFormatDisplay (Hunk u h) = displayHunk (Just u) h
  showPrim _ (Hunk u h) = storeHunk u h
  showPrim FileNameFormatDisplay (HunkMove hm) = displayHunkMove hm
  showPrim _ (HunkMove hm) = storeHunkMove hm
  showPrim _ (Manifest f (L d p)) = showManifest "manifest" d f p
  showPrim _ (Demanifest f (L d p)) = showManifest "demanifest" d f p
  showPrim _ Identity = blueText "identity"
  showPrimCtx _ _ = error "show with context not implemented"

showManifest :: String -> UUID -> UUID -> Name -> Doc
showManifest txt dir file name =
  blueText txt <+>
  formatUUID file <+>
  formatUUID dir <+>
  packedString (encodeWhiteName name)

displayHunk :: Maybe UUID -> Hunk wX wY -> Doc
displayHunk uid (H off old new) =
  blueText "hunk" <+>
  maybe (text "<nil>") formatUUID uid <+>
  text (show off) $$
  displayFileContent "-" old $$
  displayFileContent "+" new

storeHunk :: UUID -> Hunk wX wY -> Doc
storeHunk uid (H off old new) =
  text "hunk" <+>
  formatUUID uid <+>
  text (show off) $$
  storeFileContent old $$
  storeFileContent new

displayHunkMove :: HunkMove wX wY -> Doc
displayHunkMove (HM sid soff tid toff c) =
  blueText "hunkmove" <+>
  formatUUID sid <+>
  text (show soff) <+>
  formatUUID tid <+>
  text (show toff) $$
  displayFileContent "|" c

storeHunkMove :: HunkMove wX wY -> Doc
storeHunkMove (HM sid soff tid toff c) =
  text "hunkmove" <+>
  formatUUID sid <+>
  text (show soff) <+>
  formatUUID tid <+>
  text (show toff) $$
  storeFileContent c

-- TODO add some heuristics to recognize binary content
displayFileContent :: String -> FileContent -> Doc
displayFileContent pre = vcat . map (prefix pre) . showLines . linesPS
  where
    context = blueText "[...]"
    showLines [] = []
    showLines [x]
      | B.null x = []
      | otherwise = [context <> packedString x <> context]
    showLines (x:xs) =
      [context <> packedString x] ++
      map packedString (init xs) ++
      [packedString (last xs) <> context]

storeFileContent :: FileContent -> Doc
storeFileContent c =
  text "content" <+> text (show (B.length c)) $$ packedString c

formatUUID :: UUID -> Doc
formatUUID (UUID x) = packedString x
