module Darcs.Patch.FileHunk
    ( FileHunk(..), IsHunk(..), showFileHunk
    )
    where

import Darcs.Prelude

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Patch.Format ( FileNameFormat )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Show ( formatFileName )

import Darcs.Util.Printer
    ( Doc, blueText, text, lineColor, vcat, userchunkPS
    , prefix, ($$), (<+>), Color(Cyan, Magenta) )

import qualified Data.ByteString as B ( ByteString )


data FileHunk wX wY = FileHunk !AnchoredPath !Int [B.ByteString] [B.ByteString]

type role FileHunk nominal nominal

class IsHunk p where
    isHunk :: p wX wY -> Maybe (FileHunk wX wY)

showFileHunk :: FileNameFormat -> FileHunk wX wY -> Doc
showFileHunk x (FileHunk f line old new) =
           blueText "hunk" <+> formatFileName x f <+> text (show line)
        $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS old))
        $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS new))

instance Invert FileHunk where
    invert (FileHunk path line old new) = FileHunk path line new old
