-- Copyright (C) 2002-2004,2007 David Roundy
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
module Darcs.Patch.Bundle
    ( Bundle(..)
    , makeBundle
    , parseBundle
    , interpretBundle
    , readContextFile
    , minContext
    ) where

import Darcs.Prelude

import Control.Applicative ( many, (<|>) )
import Control.Monad ( (<=<) )

import qualified Data.ByteString as B
    ( ByteString
    , breakSubstring
    , concat
    , drop
    , isPrefixOf
    , null
    , splitAt
    )
import qualified Data.ByteString.Char8 as BC
    ( break
    , dropWhile
    , pack
    )

import Darcs.Patch
    ( RepoPatch
    , ApplyState
    , showPatch
    , showContextPatch
    )
import Darcs.Patch.Bracketed ( Bracketed, unBracketedFL )
import Darcs.Patch.Commute ( Commute, commuteFL )
import Darcs.Patch.Depends ( contextPatches, splitOnTag )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info
    ( PatchInfo
    , displayPatchInfo
    , piTag
    , readPatchInfo
    , showPatchInfo
    )
import Darcs.Patch.Named ( Named, fmapFL_Named )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , info
    , n2pia
    , patchInfoAndPatch
    , unavailable
    )
import Darcs.Patch.Permutations ( genCommuteWhatWeCanRL )
import Darcs.Patch.Read ( readPatch' )
import Darcs.Patch.Set
    ( PatchSet(..)
    , SealedPatchSet
    , Origin
    , appendPSFL
    )
import Darcs.Patch.Show ( ShowPatchBasic, ShowPatchFor(ForStorage) )
import Darcs.Patch.Witnesses.Ordered
    ( (:>)(..)
    , FL(..)
    , RL(..)
    , mapFL
    , mapFL_FL
    , mapRL
    , reverseFL
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd, unsafeCoercePStart )

import Darcs.Util.ByteString
    ( dropSpace
    , mmapFilePS
    , betweenLinesPS
    )
import Darcs.Util.Hash ( sha1PS, sha1Show )
import Darcs.Util.Parser
    ( Parser
    , lexString
    , lexWord
    , optional
    , parse
    )
import Darcs.Util.Printer
    ( Doc
    , ($$)
    , newline
    , packedString
    , renderPS
    , renderString
    , text
    , vcat
    , vsep
    )
import Darcs.Util.Tree( Tree )
import Darcs.Util.Tree.Monad( virtualTreeIO )


-- | A 'Bundle' is a context together with some patches. The context
-- consists of unavailable patches.
data Bundle rt p wX wY where
  Bundle :: (FL (PatchInfoAnd rt p) :> FL (PatchInfoAnd rt p)) wX wY
         -> Bundle rt p wX wY

-- | Interpret a 'Bundle' in the context of a 'PatchSet'. This means we
-- match up a possible tag in the context of the 'Bundle'. This fails if
-- the tag couldn't be found.
interpretBundle :: Commute p
                => PatchSet rt p Origin wT
                -> Bundle rt p wA wB
                -> Either String (PatchSet rt p Origin wB)
interpretBundle ref (Bundle (context :> patches)) =
  flip appendPSFL patches <$> interpretContext ref context

-- | Create a b16 encoded SHA1 of a given a FL of named patches. This allows us
-- to ensure that the patches in a received bundle have not been modified in
-- transit.
hashBundle :: (PatchListFormat p, ShowPatchBasic p) => FL (Named p) wX wY
           -> B.ByteString
hashBundle to_be_sent =
    sha1Show $ sha1PS $ renderPS $
        vcat (mapFL (showPatch ForStorage) to_be_sent) <> newline

makeBundle :: (ApplyState p ~ Tree, RepoPatch p) => Maybe (Tree IO)
           -> PatchSet rt p wStart wX -> FL (Named p) wX wY -> IO Doc
makeBundle state repo to_be_sent
  | _ :> context <- contextPatches repo =
    format context <$>
      case state of
        Just tree ->
          fst <$> virtualTreeIO (showContextPatch ForStorage to_be_sent) tree
        Nothing -> return (vsep $ mapFL (showPatch ForStorage) to_be_sent)
  where
    format context patches =
      text ""
      $$ text "New patches:"
      $$ text ""
      $$ patches
      $$ text ""
      $$ text "Context:"
      $$ text ""
      $$ vcat (mapRL (showPatchInfo ForStorage . info) context)
      $$ text "Patch bundle hash:"
      $$ packedString (hashBundle to_be_sent)
      $$ text ""

hashFailureMessage :: String
hashFailureMessage =
  "Patch bundle failed hash!\n\
  \This probably means that the patch has been corrupted by a mailer.\n\
  \The most likely culprit is CRLF newlines."

parseBundle :: RepoPatch p
            => B.ByteString -> Either String (Sealed (Bundle rt p wX))
parseBundle =
    fmap fst . parse pUnsignedBundle . dropInitialTrash . decodeGpgClearsigned
  where
    dropInitialTrash s =
      case BC.break (== '\n') (dropSpace s) of
        (line,rest)
          | contextName `B.isPrefixOf` line || patchesName `B.isPrefixOf` line -> s
          | B.null rest -> rest
          | otherwise -> dropInitialTrash rest

pUnsignedBundle :: forall rt p wX. RepoPatch p => Parser (Sealed (Bundle rt p wX))
pUnsignedBundle = pContextThenPatches <|> pPatchesThenContext
  where
    packBundle context patches =
      Sealed $ Bundle $ (unavailablePatchesFL (reverse context)) :>
        (mapFL_FL (n2pia . fmapFL_Named unBracketedFL) patches)
    -- Is this a legacy format?
    pContextThenPatches = do
      context <- pContext
      Sealed patches <- pPatches
      return $ packBundle context patches
    pPatchesThenContext = do
      Sealed patches <- pPatches
      context <- pContext
      mBundleHash <- optional pBundleHash
      case mBundleHash of
        Just bundleHash -> do
          let realHash = hashBundle patches
          if realHash == bundleHash
            then return $ packBundle context patches
            else fail hashFailureMessage
        Nothing -> return $ packBundle context patches

pBundleHash :: Parser B.ByteString
pBundleHash = lexString bundleHashName >> lexWord

bundleHashName :: B.ByteString
bundleHashName = BC.pack "Patch bundle hash:"

unavailablePatchesFL :: [PatchInfo] -> FL (PatchInfoAnd rt p) wX wY
unavailablePatchesFL = foldr ((:>:) . piUnavailable) (unsafeCoercePEnd NilFL)
  where
    piUnavailable i = patchInfoAndPatch i . unavailable $
      "Patch not stored in patch bundle:\n" ++ renderString (displayPatchInfo i)

pContext :: Parser [PatchInfo]
pContext = lexString contextName >> many readPatchInfo

contextName :: B.ByteString
contextName = BC.pack "Context:"

pPatches :: RepoPatch p => Parser (Sealed (FL (Named (Bracketed p)) wX))
pPatches = lexString patchesName >> readPatch'

patchesName :: B.ByteString
patchesName = BC.pack "New patches:"

readContextFile :: Commute p
                => PatchSet rt p Origin wX
                -> FilePath
                -> IO (SealedPatchSet rt p Origin)
readContextFile ref = fmap Sealed . (parseAndInterpret <=< mmapFilePS)
  where
    parseAndInterpret =
      either fail return . (interpretContext ref <=< parseContextFile)

-- | Interpret a context file in the context of a 'PatchSet'. This means we
-- match up a possible tag. This fails if the tag couldn't be found.
interpretContext :: Commute p
                 => PatchSet rt p Origin wT
                 -> FL (PatchInfoAnd rt p) wA wB
                 -> Either String (PatchSet rt p Origin wB)
interpretContext ref context =
  case context of
    tag :>: rest
      | Just tagname <- piTag (info tag) ->
        case splitOnTag (info tag) ref of
          Nothing ->
            Left $ "Cannot find tag " ++ tagname ++ " from context in our repo"
          Just (PatchSet ts _) ->
            Right $ PatchSet ts (unsafeCoercePStart (reverseFL rest))
    _ -> Right $ PatchSet NilRL (unsafeCoercePStart (reverseFL context))

parseContextFile :: B.ByteString
                 -> Either String (FL (PatchInfoAnd rt p) wX wY)
parseContextFile =
    fmap fst . parse pUnsignedContext . decodeGpgClearsigned
  where
    pUnsignedContext = unavailablePatchesFL . reverse <$> pContext

-- | Minimize the context of an 'FL' of patches to be packed into a bundle.
minContext :: (RepoPatch p)
           => PatchSet rt p wStart wB -- context to be minimized
           -> FL (PatchInfoAnd rt p) wB wC
           -> Sealed ((PatchSet rt p :> FL (PatchInfoAnd rt p)) wStart)
minContext (PatchSet behindTag topCommon) to_be_sent =
  case genCommuteWhatWeCanRL commuteFL (topCommon :> to_be_sent) of
    (c :> to_be_sent' :> _) -> seal (PatchSet behindTag c :> to_be_sent') 

-- TODO shouldn't we verify the signature? That is, pipe the input through
-- "gpg --verify -o-"? This would also let gpg handle their own mangling.

-- | Decode gpg clearsigned file content.
decodeGpgClearsigned :: B.ByteString -> B.ByteString
decodeGpgClearsigned input =
  case betweenLinesPS startSignedName endSignedName input of
    Nothing -> input
    Just signed -> removeGpgDashes (dropHashType signed)
  where
    -- Note that B.concat is optimized to avoid unnecessary work, in particular
    -- concatenating slices that were originally adjacent involves no extra
    -- copying, and allocation of the result buffer is done only once.
    removeGpgDashes = B.concat . splitGpgDashes
    splitGpgDashes s =
      case B.breakSubstring newline_dashes s of
        (before, rest)
          | B.null rest -> [s]
          | (keep, after) <- B.splitAt 2 rest ->
              before : keep : splitGpgDashes (B.drop 2 after)
    newline_dashes = BC.pack "\n- -"
    dropHashType s =
      case B.breakSubstring hashTypeName s of
        (_, rest)
          | B.null rest -> s
          | otherwise -> dropSpace $ BC.dropWhile (/= '\n') rest
    hashTypeName = BC.pack "Hash:"
    startSignedName = BC.pack "-----BEGIN PGP SIGNED MESSAGE-----"
    endSignedName = BC.pack "-----BEGIN PGP SIGNATURE-----"
