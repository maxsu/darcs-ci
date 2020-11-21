module Darcs.Patch.Summary
    ( plainSummary
    , plainSummaryFL
    , plainSummaryPrim
    , plainSummaryPrims
    , xmlSummary
    , Summary(..)
    , ConflictState(..)
    , IsConflictedPrim(..)
    , listConflictedFiles
    ) where

import Darcs.Prelude

import Data.List.Ordered ( nubSort )
import Data.Maybe ( catMaybes )

import Darcs.Patch.Format ( FileNameFormat(FileNameFormatDisplay) )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Prim ( PrimDetails(..) )
import Darcs.Patch.Show ( formatFileName )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )
import Darcs.Patch.Witnesses.Show

import Darcs.Util.Path ( AnchoredPath, anchorPath )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , (<+>)
    , empty
    , minus
    , plus
    , text
    , vcat
    )

-- | This type tags a patch with a 'ConflictState' and also hides the context
-- witnesses (as in 'Sealed2'), so we can put them in a list.
data IsConflictedPrim prim where
    IsC :: !ConflictState -> !(prim wX wY) -> IsConflictedPrim prim
data ConflictState = Okay | Conflicted | Duplicated deriving ( Eq, Ord, Show, Read)

class Summary p where
    conflictedEffect :: p wX wY -> [IsConflictedPrim (PrimOf p)]

instance Summary p => Summary (FL p) where
    conflictedEffect = concat . mapFL conflictedEffect

instance Show2 prim => Show (IsConflictedPrim prim) where
    showsPrec d (IsC cs prim) =
        showParen (d > appPrec) $
            showString "IsC " . showsPrec (appPrec + 1) cs .
            showString " " . showsPrec2 (appPrec + 1) prim

listConflictedFiles
  :: (Summary p, PatchInspect (PrimOf p)) => p wX wY -> [AnchoredPath]
listConflictedFiles =
    nubSort . concat . catMaybes . map conflictedFiles . conflictedEffect
  where
    conflictedFiles (IsC Conflicted p) = Just (listTouchedFiles p)
    conflictedFiles _ = Nothing

plainSummaryPrim :: PrimDetails prim => prim wX wY -> Doc
plainSummaryPrim = vcat . map (summChunkToLine False) . genSummary . (:[]) . IsC Okay

plainSummaryPrims :: PrimDetails prim => Bool -> FL prim wX wY -> Doc
plainSummaryPrims machineReadable =
 vcat . map (summChunkToLine machineReadable) . genSummary . mapFL (IsC Okay)

plainSummary :: (Summary e, PrimDetails (PrimOf e)) => e wX wY -> Doc
plainSummary = vcat . map (summChunkToLine False) . genSummary . conflictedEffect

plainSummaryFL :: (Summary e, PrimDetails (PrimOf e)) => FL e wX wY -> Doc
plainSummaryFL = vcat . map (summChunkToLine False) . genSummary . concat . mapFL conflictedEffect

xmlSummary :: (Summary p, PrimDetails (PrimOf p)) => p wX wY -> Doc
xmlSummary p = text "<summary>"
             $$ (vcat . map summChunkToXML . genSummary . conflictedEffect $ p)
             $$ text "</summary>"

-- Yuck duplicated code below...
escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ strReplace x y zs
  | otherwise = z : strReplace x y zs
-- end yuck duplicated code.

-- | High-level representation of a piece of patch summary
data SummChunk = SummChunk SummDetail ConflictState
   deriving (Ord, Eq)

genSummary :: forall p . PrimDetails p => [IsConflictedPrim p] -> [SummChunk]
genSummary p
    = combine $ concatMap s2 p
    where s2 :: IsConflictedPrim p -> [SummChunk]
          s2 (IsC c x) = map (`SummChunk` c) $ summarizePrim x
          combine (x1@(SummChunk d1 c1) : x2@(SummChunk d2 c2) : ss)
              = case combineDetail d1 d2 of
                  Nothing -> x1 : combine (x2:ss)
                  Just d3 -> combine $ SummChunk d3 (combineConflictStates c1 c2) : ss
          combine (x:ss) = x  : combine ss
          combine [] = []
          --
          combineDetail (SummFile o1 f1 r1 a1 x1) (SummFile o2 f2 r2 a2 x2) | f1 == f2 =
            do o3 <- combineOp o1 o2
               return $ SummFile o3 f1 (r1 + r2) (a1 + a2) (x1 + x2)
          combineDetail _ _ = Nothing
          --
          combineConflictStates Conflicted _ = Conflicted
          combineConflictStates _ Conflicted = Conflicted
          combineConflictStates Duplicated _ = Duplicated
          combineConflictStates _ Duplicated = Duplicated
          combineConflictStates Okay Okay = Okay
          -- Don't combine AddFile and RmFile: (maybe an old revision of) darcs
          -- allows a single patch to add and remove the same file, see issue 185
          combineOp SummAdd SummRm  = Nothing
          combineOp SummRm  SummAdd = Nothing
          combineOp SummAdd _ = Just SummAdd
          combineOp _ SummAdd = Just SummAdd
          combineOp SummRm  _ = Just SummRm
          combineOp _ SummRm  = Just SummRm
          combineOp SummMod SummMod = Just SummMod

summChunkToXML :: SummChunk -> Doc
summChunkToXML (SummChunk detail c) =
 case detail of
   SummRmDir f  -> xconf c "remove_directory" (xfn f)
   SummAddDir f -> xconf c "add_directory"    (xfn f)
   SummFile SummRm  f _ _ _ -> xconf c "remove_file" (xfn f)
   SummFile SummAdd f _ _ _ -> xconf c "add_file"    (xfn f)
   SummFile SummMod f r a x -> xconf c "modify_file" $ xfn f <> xrm r <> xad a <> xrp x
   SummMv f1 f2  -> text "<move from=\"" <> xfn f1
                      <> text "\" to=\"" <> xfn f2 <> text"\"/>"
   SummNone      -> empty
 where
   xconf Okay t x       = text ('<':t++">") $$ x $$ text ("</"++t++">")
   xconf Conflicted t x = text ('<':t++" conflict='true'>") $$ x $$ text ("</"++t++">")
   xconf Duplicated t x = text ('<':t++" duplicate='true'>") $$ x $$ text ("</"++t++">")
   xfn = escapeXML . anchorPath ""
   --
   xad 0 = empty
   xad a = text "<added_lines num='" <> text (show a) <> text "'/>"
   xrm 0 = empty
   xrm a = text "<removed_lines num='" <> text (show a) <> text "'/>"
   xrp 0 = empty
   xrp a = text "<replaced_tokens num='" <> text (show a) <> text "'/>"

summChunkToLine :: Bool -> SummChunk -> Doc
summChunkToLine machineReadable (SummChunk detail c) =
  case detail of
   SummRmDir f   -> lconf c "R" $ formatFileName FileNameFormatDisplay f <> text "/"
   SummAddDir f  -> lconf c "A" $ formatFileName FileNameFormatDisplay f <> text "/"
   SummFile SummRm  f _ _ _ -> lconf c "R" $ formatFileName FileNameFormatDisplay f
   SummFile SummAdd f _ _ _ -> lconf c "A" $ formatFileName FileNameFormatDisplay f
   SummFile SummMod f r a x
     | machineReadable -> lconf c "M" $ formatFileName FileNameFormatDisplay f
     | otherwise       -> lconf c "M" $ formatFileName FileNameFormatDisplay f <+> rm r <+> ad a <+> rp x
   SummMv f1 f2
     | machineReadable -> text "F " <> formatFileName FileNameFormatDisplay f1
                       $$ text "T " <> formatFileName FileNameFormatDisplay f2
     | otherwise       -> text " "    <> formatFileName FileNameFormatDisplay f1
                       <> text " -> " <> formatFileName FileNameFormatDisplay f2
   SummNone -> case c of
               Okay -> empty
               _    -> lconf c ""  empty
  where
   lconf Okay       t x = text t <+> x
   lconf Conflicted t x = text (t ++ "!") <+> x
   lconf Duplicated t x
     | machineReadable = text t <+> x
     | otherwise       = text t <+> x <+> text "duplicate"
   --
   ad 0 = empty
   ad a = plus <> text (show a)
   rm 0 = empty
   rm a = minus <> text (show a)
   rp 0 = empty
   rp a = text "r" <> text (show a)
