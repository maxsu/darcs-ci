{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Show ( showPatch_ ) where

import Darcs.Prelude

import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatchFor(..) )
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )

import Darcs.Util.Printer ( Doc, text, blueText, ($$), (<+>) )


showPatch_ :: ShowPatchBasic prim => prim wX wY -> Doc
showPatch_ = showPatch ForDisplay

showMerger :: ShowPatchBasic prim
           => ShowPatchFor
           -> String
           -> RepoPatchV1 prim wA wB
           -> RepoPatchV1 prim wD wE
           -> Doc
showMerger f merger_name p1 p2 =
    blueText merger_name <+> text "0.0" <+> blueText "("
                           $$ showPatch f p1
                           $$ showPatch f p2
                           $$ blueText ")"

instance ShowPatchBasic prim => ShowPatchBasic (RepoPatchV1 prim) where
    showPatch f (PP p) = showPatch f p
    showPatch f (Merger _ _ p1 p2) = showMerger f "merger" p1 p2
    showPatch f (Regrem _ _ p1 p2) = showMerger f "regrem" p1 p2
