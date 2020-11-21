--  Copyright (C) 2002-2005 David Roundy
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

module Darcs.Patch.Show
     ( ShowPatchBasic(..)
     , displayPatch
     , ShowPatchFor(..)
     , ShowPatch(..)
     , ShowContextPatch(..)
     , formatFileName
     ) where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC ( unpack )

import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Witnesses.Ordered ( FL, mapFL )

import Darcs.Util.ByteString ( packStringToUTF8, encodeLocale )
import Darcs.Util.English ( plural, Noun(Noun) )
import Darcs.Util.Path ( AnchoredPath, encodeWhite, anchorPath )
import Darcs.Util.Printer ( Doc, vcat, text, packedString )

data ShowPatchFor = ForDisplay | ForStorage

displayPatch :: ShowPatchBasic p => p wX wY -> Doc
displayPatch p = showPatch ForDisplay p

class ShowPatchBasic p where
    showPatch :: ShowPatchFor -> p wX wY -> Doc

class ShowPatchBasic p => ShowContextPatch p where
    -- | showContextPatch is used to add context to a patch, as diff
    -- -u does. Thus, it differs from showPatch only for hunks. It is
    -- used for instance before putting it into a bundle. As this
    -- unified context is not included in patch representation, this
    -- requires access to the tree.
    showContextPatch :: (ApplyMonad (ApplyState p) m)
                     => ShowPatchFor -> p wX wY -> m Doc

-- | This class is used only for user interaction, not for storage. The default
-- implementations for 'description' and 'content' are suitable only for
-- 'PrimPatch' and 'RepoPatch' types. Logically, 'description' should default
-- to 'mempty' while 'content' should default to 'displayPatch'. We define them
-- the other way around so that 'Darcs.UI.PrintPatch.showFriendly' gives
-- reasonable results for all patch types.
class ShowPatchBasic p => ShowPatch p where
    content :: p wX wY -> Doc
    content = mempty

    description :: p wX wY -> Doc
    description = displayPatch

    summary :: p wX wY -> Doc

    summaryFL :: FL p wX wY -> Doc
    summaryFL = vcat . mapFL summary

    thing :: p wX wY -> String
    thing _ = "patch"

    things :: p wX wY -> String
    things x = plural (Noun $ thing x) ""

-- | Format a 'AnchoredPath' to a 'Doc' according to the given 'FileNameFormat'.
--
-- NOTE: This is not only used for display but also to format patch files. This is
--       why we have to do the white space encoding here.
--       See 'Darcs.Repository.Hashed.writePatchIfNecessary'.
--
-- Besides white space encoding, for 'FileNameFormatV2' we just pack it into a 'Doc'. For
-- 'FileNameFormatV1' we must emulate the non-standard darcs-1 encoding of file paths: it
-- is an UTF8 encoding of the raw byte stream, interpreted as code points.
--
-- See also 'Darcs.Patch.Show.readFileName'.
formatFileName :: FileNameFormat -> AnchoredPath -> Doc
formatFileName FileNameFormatV1 = packedString . packStringToUTF8 . BC.unpack .
                            encodeLocale . encodeWhite . ap2fp
formatFileName FileNameFormatV2 = text . encodeWhite . ap2fp
formatFileName FileNameFormatDisplay = text . ap2fp

ap2fp :: AnchoredPath -> FilePath
ap2fp ap = '.' : '/' : anchorPath "" ap
