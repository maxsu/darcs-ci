-- Copyright (C) 2002-2004 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}

module Darcs.Patch.Viewing
    ( showContextHunk
    ) where

import Darcs.Prelude hiding ( readFile )

import Control.Applicative( (<$>) )
import qualified Data.ByteString as B ( null )
import Darcs.Util.Tree ( Tree )
import Darcs.Util.Tree.Monad ( virtualTreeMonad )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad ( getApplyState,
                                ApplyMonad(..), ApplyMonadTree(..), toTree )
import Darcs.Patch.FileHunk ( IsHunk(..), FileHunk(..), showFileHunk )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(..),
                            FileNameFormat(..) )
import Darcs.Patch.Show
    ( ShowPatchBasic(..), ShowPatch(..)
    , formatFileName, ShowPatchFor(..), ShowContextPatch(..) )
import Darcs.Patch.Witnesses.Ordered ( RL(..), FL(..), mapFL, mapFL_FL,
                                       reverseRL, concatFL )
import Darcs.Util.ByteString ( linesPS )
import Darcs.Util.Printer ( Doc, empty, vcat, text, blueText, Color(Cyan, Magenta),
                 lineColor, ($$), (<+>), prefix, userchunkPS )

showContextSeries :: forall p m wX wY . (Apply p, ShowContextPatch p, IsHunk p,
                                         ApplyMonad (ApplyState p) m)
                  => ShowPatchFor -> FileNameFormat -> FL p wX wY -> m Doc
showContextSeries use fmt = scs Nothing
  where
    scs :: forall wWw wXx wYy . Maybe (FileHunk wWw wXx) -> FL p wXx wYy -> m Doc
    scs pold (p :>: ps) = do
        (_, s') <- nestedApply (apply p) =<< getApplyState
        case isHunk p of
            Nothing -> do
                a <- showContextPatch use p
                b <- nestedApply (scs Nothing ps) s'
                return $ a $$ fst b
            Just fh -> case ps of
                NilFL -> fst <$> liftApply (cool pold fh Nothing) s'
                (p2 :>: _) -> do
                    a <- fst <$> liftApply (cool pold fh (isHunk p2)) s'
                    b <- nestedApply (scs (Just fh) ps) s'
                    return $ a $$ fst b
    scs _ NilFL = return empty

    cool :: Maybe (FileHunk wA wB) -> FileHunk wB wC -> Maybe (FileHunk wC wD)
         -> (ApplyState p) (ApplyMonadBase m) -> (ApplyMonadBase m) Doc
    cool pold fh ps s =
        fst <$> virtualTreeMonad (coolContextHunk fmt pold fh ps) (toTree s)

showContextHunk :: (ApplyMonad Tree m) => FileNameFormat -> FileHunk wX wY -> m Doc
showContextHunk fmt h = coolContextHunk fmt Nothing h Nothing

coolContextHunk :: (ApplyMonad Tree m)
                => FileNameFormat
                -> Maybe (FileHunk wA wB) -> FileHunk wB wC
                -> Maybe (FileHunk wC wD) -> m Doc
coolContextHunk fmt prev fh@(FileHunk f l o n) next = do
    have <- mDoesFileExist f
    f_content <- if have then Just `fmap` mReadFilePS f else return Nothing
    case linesPS `fmap` f_content of
        -- FIXME This is a weird error...
        Nothing -> return $ showFileHunk fmt fh
        Just ls ->
            let pre = take numpre $ drop (l - numpre - 1) ls
                cleanedls = case reverse ls of
                    (x : xs)
                        | B.null x -> reverse xs
                    _ -> ls
                post = take numpost $ drop (max 0 $ l+length o-1) cleanedls in
            return $
                blueText "hunk" <+> formatFileName fmt f
                    <+> text (show l)
                $$ prefix " " (vcat $ map userchunkPS pre)
                $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS o))
                $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS n))
                $$ prefix " " (vcat $ map userchunkPS post)
  where
    numpre = case prev of
        Just (FileHunk f' lprev _ nprev)
            | f' == f && l - (lprev + length nprev + 3) < 3 && lprev < l
            -> max 0 $ l - (lprev + length nprev + 3)
        _ -> if l >= 4 then 3 else l - 1

    numpost = case next of
        Just (FileHunk f' lnext _ _)
            | f' == f && lnext < l + length n + 4 && lnext > l
            -> lnext - (l + length n)
        _ -> 3

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (FL p) where
    showPatch ForDisplay = vcat . mapFL (showPatch ForDisplay)
    showPatch ForStorage = showPatchInternal patchListFormat
      where
        showPatchInternal :: ListFormat p -> FL p wX wY -> Doc
        showPatchInternal ListFormatV1 (p :>: NilFL) = (showPatch ForStorage) p
        showPatchInternal ListFormatV1 NilFL = blueText "{" $$ blueText "}"
        showPatchInternal ListFormatV1 ps = blueText "{"
                                            $$ vcat (mapFL (showPatch ForStorage) ps)
                                            $$ blueText "}"
        showPatchInternal ListFormatV2 ps = vcat (mapFL (showPatch ForStorage) ps)
        showPatchInternal ListFormatDefault ps = vcat (mapFL (showPatch ForStorage) ps)
        showPatchInternal ListFormatV3 ps = vcat (mapFL (showPatch ForStorage) ps)

instance (Apply p, IsHunk p, PatchListFormat p, ShowContextPatch p)
        => ShowContextPatch (FL p) where
    showContextPatch ForDisplay = showContextSeries ForDisplay FileNameFormatDisplay
    showContextPatch ForStorage = showContextPatchInternal patchListFormat
      where
        showContextPatchInternal :: (ApplyMonad (ApplyState (FL p)) m)
                                 => ListFormat p -> FL p wX wY -> m Doc
        showContextPatchInternal ListFormatV1 (p :>: NilFL) =
            showContextPatch ForStorage p
        showContextPatchInternal ListFormatV1 NilFL =
            return $ blueText "{" $$ blueText "}"
        showContextPatchInternal ListFormatV1 ps = do
            x <- showContextSeries ForStorage FileNameFormatV1 ps
            return $ blueText "{" $$ x $$ blueText "}"
        showContextPatchInternal ListFormatV2 ps = showContextSeries ForStorage FileNameFormatV2 ps
        showContextPatchInternal ListFormatDefault ps = showContextSeries ForStorage FileNameFormatV2 ps
        showContextPatchInternal ListFormatV3 ps = return $ showPatch ForStorage ps

instance (PatchListFormat p, ShowPatch p) => ShowPatch (FL p) where
    content = vcat . mapFL content

    description = vcat . mapFL description

    summary = summaryFL

    summaryFL = summaryFL . concatFL

    thing x = thing (helperx x) ++ "s"
      where
        helperx :: FL a wX wY -> a wX wY
        helperx _ = undefined

    things = thing

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (RL p) where
    showPatch f = showPatch f . reverseRL

instance (ShowContextPatch p, Apply p, IsHunk p, PatchListFormat p)
        => ShowContextPatch (RL p) where
    showContextPatch use = showContextPatch use . reverseRL

instance (PatchListFormat p, ShowPatch p) => ShowPatch (RL p) where
    content = content . reverseRL

    description = description . reverseRL

    summary = summary . reverseRL

    summaryFL = summaryFL . mapFL_FL reverseRL

    thing = thing . reverseRL

    things = things . reverseRL
