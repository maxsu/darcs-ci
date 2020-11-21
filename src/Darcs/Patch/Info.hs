-- Copyright (C) 2002-2003 David Roundy
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

module Darcs.Patch.Info
    ( PatchInfo(..) -- constructor and fields exported *only for tests*
    , rawPatchInfo  -- exported *only for tests*
    , patchinfo
    , addJunk
    , replaceJunk
    , makePatchname
    , readPatchInfo
    , justName
    , justAuthor
    , justLog
    , displayPatchInfo
    , toXml
    , toXmlShort
    , piDate
    , piDateString
    , piName
    , piRename
    , piAuthor
    , piTag
    , piLog
    , showPatchInfo
    , isTag
    , escapeXML
    , validDate
    , validLog
    , validAuthor
    , validDatePS
    , validLogPS
    , validAuthorPS
    ) where

import Darcs.Prelude

import Data.Char ( isAscii )
import Crypto.Random ( seedNew, seedToInteger )
import Numeric ( showHex )
import Control.Monad ( when, unless, void )

import Darcs.Util.ByteString
    ( decodeLocale
    , packStringToUTF8
    , unlinesPS
    , unpackPSFromUTF8
    )
import qualified Darcs.Util.Parser as RM ( take )
import Darcs.Util.Parser as RM ( skipSpace, char,
                                      takeTill, anyChar, Parser,
                                      option,
                                      takeTillChar,
                                      linesStartingWithEndingWith)
import Darcs.Patch.Show ( ShowPatchFor(..) )
import qualified Data.ByteString       as B  (length, splitAt, null
                                             ,isPrefixOf, tail, concat
                                             ,empty, head, cons, append
                                             ,ByteString )
import qualified Data.ByteString.Char8 as BC
    ( index, head, notElem, all, unpack, pack )
import Data.List( isPrefixOf )

import Darcs.Util.Printer ( Doc, packedString,
                 empty, ($$), (<+>), vcat, text, cyanText, blueText, prefix )
import Darcs.Util.IsoDate ( readUTCDate )
import System.Time ( CalendarTime, calendarTimeToString, toClockTime,
                     toCalendarTime )
import System.IO.Unsafe ( unsafePerformIO )
import Darcs.Util.Hash ( sha1PS, SHA1 )
import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Show ( appPrec )

import Darcs.Test.TestOnly ( TestOnly )

{- |
A PatchInfo value contains the metadata of a patch. The date, name, author
and log fields are UTF-8 encoded text in darcs 2.4 and later, and just
sequences of bytes (decoded with whatever is the locale when displayed) in
earlier darcs.

The members with names that start with '_' are not supposed to be used
directly in code that does not care how the patch info is stored.

@_piLegacyIsInverted@:

Historically, the @isInverted@ flag was used to indicate that a Named patch
was inverted.

We no longer support direct inversion of 'Darcs.Patch.Named.Named' patches,
except sometimes via the 'Darcs.Patch.Invertible.Invertible' wrapper which
tracks inversion in the wrapper.

However, going even further back in time, inverted patches could be written
out by @darcs rollback@. This was changed in 2008 so any patches on disk
with this flag set would have been written by a darcs from prior to then.
As they still exist, including in the darcs repository itself, we need
to support them.

As far as current darcs is concerned, the flag should be treated like any
other field in 'PatchInfo' apart from never being set freshly:

 - There is no semantic relationship between a 'PatchInfo' with
   @piLegacyIsInverted = False@ and the same 'PatchInfo' with
   @piLegacyIsInverted = True@. For example they are not inverses of each
   other.

- New or amended patches should never be written out with
  @_piLegacyIsInverted = True@.

 - We do need to maintain backwards compatibility so we take care to
   preserve things like the hash, on-disk format etc.

- A patch with @_piLegacyIsInverted = True@ should work with all the
  normal darcs operations.

The flag is completely separate and orthogonal to the tracking of
explicit inversion in the 'Darcs.Patch.Invertible.Invertible' wrapper.
The 'Darcs.Patch.Invertible.Invertible' wrapper
is only used in memory and never stored to disk so there should be no
confusion when reading a patch from disk. Within the codebase they
serve completely different purposes and should not interact at all.
-}
data PatchInfo =
  PatchInfo { _piDate    :: !B.ByteString
            , _piName    :: !B.ByteString
            , _piAuthor  :: !B.ByteString
            , _piLog     :: ![B.ByteString]
              -- | See the long description of this field in the
              -- docs above.
            , _piLegacyIsInverted :: !Bool
            }
  deriving (Eq,Ord)

instance Show PatchInfo where
    showsPrec d (PatchInfo date name author log inverted) =
        showParen (d > appPrec) $
            showString "rawPatchInfo " . showsPrec (appPrec + 1) date .
            showString " " . showsPrec (appPrec + 1) name .
            showString " " . showsPrec (appPrec + 1) author .
            showString " " . showsPrec (appPrec + 1) log .
            showString " " . showsPrec (appPrec + 1) inverted

-- Validation

-- We need these functions to ensure that we can parse the
-- result of showPatchInfo.

validDate :: String -> Bool
validDate = all validCharForDate

validDatePS :: B.ByteString -> Bool
validDatePS = BC.all validCharForDate

-- | The isAscii limitation is due to the use of BC.pack below.
validCharForDate :: Char -> Bool
validCharForDate c = isAscii c && c /= '\n' && c /= ']'

validLog :: String -> Bool
validLog = notElem '\n'

validLogPS :: B.ByteString -> Bool
validLogPS = BC.notElem '\n'

validAuthor :: String -> Bool
validAuthor = notElem '*'

validAuthorPS :: B.ByteString -> Bool
validAuthorPS = BC.notElem '*'

rawPatchInfo
  :: TestOnly
  => String -> String -> String -> [String] -> Bool -> PatchInfo
rawPatchInfo = rawPatchInfoInternal

rawPatchInfoInternal :: String -> String -> String -> [String] -> Bool -> PatchInfo
rawPatchInfoInternal date name author log inverted =
    PatchInfo { _piDate     = BC.pack $ validateDate date
              , _piName     = packStringToUTF8 $ validateName name
              , _piAuthor   = packStringToUTF8 $ validateAuthor author
              , _piLog      = map (packStringToUTF8 . validateLog) log
              , _piLegacyIsInverted  = inverted
              }
  where
    validateAuthor = validate validAuthor "author"
    validateName = validate validLog "patch name"
    validateLog = validate validLog "log line"
    validateDate = validate validDate "date"
    validate test meta x =
      if test x then x else error (unwords ["invalid",meta,show x])

-- | @patchinfo date name author log@ constructs a new 'PatchInfo' value
-- with the given details, automatically assigning an Ignore-this header
-- to guarantee the patch is unique.  The function does not verify
-- the date string's sanity.
patchinfo :: String -> String -> String -> [String] -> IO PatchInfo
patchinfo date name author log =
    addJunk $ rawPatchInfoInternal date name author log False

-- | addJunk adds a line that contains a random number to make the patch
--   unique.
addJunk :: PatchInfo -> IO PatchInfo
addJunk pinf =
    do x <- seedToInteger <$> seedNew
       -- Note: this is now 40 bytes long compare to the 32 we had before
       when (_piLog pinf /= ignoreJunk (_piLog pinf)) $
            do putStrLn $ "Lines beginning with 'Ignore-this: ' " ++
                          "will not be shown when displaying a patch."
               confirmed <- promptYorn "Proceed? "
               unless confirmed $ fail "User cancelled because of Ignore-this."
       return $ pinf { _piLog = BC.pack (head ignored++showHex x ""):
                                 _piLog pinf }

replaceJunk :: PatchInfo -> IO PatchInfo
replaceJunk pi@(PatchInfo {_piLog=log}) = addJunk $ pi{_piLog = ignoreJunk log}

ignored :: [String] -- this is a [String] so we can change the junk header.
ignored = ["Ignore-this: "]

ignoreJunk :: [B.ByteString] -> [B.ByteString]
ignoreJunk = filter isnt_ignored
    where isnt_ignored x = doesnt_start_with x (map BC.pack ignored) -- TODO
          doesnt_start_with x ys = not $ any (`B.isPrefixOf` x) ys


-- * Patch info formatting

-- | Get the name, including an "UNDO: " prefix if the patch is
-- a legacy inverted patch.
justName :: PatchInfo -> String
justName pinf =
  if _piLegacyIsInverted pinf
    then "UNDO: " ++ nameString
    else nameString
  where nameString = metadataToString (_piName pinf)

-- | Returns the author of a patch.
justAuthor :: PatchInfo -> String
justAuthor =  metadataToString . _piAuthor

justLog :: PatchInfo -> String
justLog = unlines . map BC.unpack . _piLog

displayPatchInfo :: PatchInfo -> Doc
displayPatchInfo pi =
    cyanText "patch " <> cyanText (show $ makePatchname pi)
 $$ text "Author: " <> text (piAuthor pi)
 $$ text "Date:   " <> text (friendlyD $ _piDate pi)
 $$ hfn (piName pi)
 $$ vcat (map ((text "  " <>) . text) (piLog pi))
  where hfn x = case piTag pi of
                Nothing -> inverted <+> text x
                Just t -> text "  tagged" <+> text t
        inverted = if _piLegacyIsInverted pi then text "  UNDO:" else text "  *"

-- | Returns the name of the patch. Unlike 'justName', it does not preprend
--   "UNDO: " to the name if the patch has the legacy inverted flag set.
piName :: PatchInfo -> String
piName = metadataToString . _piName

piRename :: PatchInfo -> String -> PatchInfo
piRename x n = x { _piName = packStringToUTF8 n }

-- | Returns the author of a patch.
piAuthor :: PatchInfo -> String
piAuthor = metadataToString . _piAuthor

isTag :: PatchInfo -> Bool
isTag pinfo = "TAG " `isPrefixOf` justName pinfo

-- | Read the date from raw patch (meta) data and convert it to UTC.
-- The raw data may contain timezone info. This is for compatibiltity
-- with patches that were created before 2003-11, when darcs still
-- created patches that contained localized date strings.
readPatchDate :: B.ByteString -> CalendarTime
readPatchDate = readUTCDate . BC.unpack

piDate :: PatchInfo -> CalendarTime
piDate = readPatchDate . _piDate

piDateString :: PatchInfo -> String
piDateString = BC.unpack . _piDate

-- | Get the log message of a patch.
piLog :: PatchInfo -> [String]
piLog = map metadataToString . ignoreJunk . _piLog

-- | Get the tag name, if the patch is a tag patch.
piTag :: PatchInfo -> Maybe String
piTag pinf =
    if l == t
      then Just $ metadataToString r
      else Nothing
    where (l, r) = B.splitAt (B.length t) (_piName pinf)
          t = BC.pack "TAG "

-- | Convert a metadata ByteString to a string. It first tries to convert
--   using UTF-8, and if that fails, tries the locale encoding.
--   We try UTF-8 first because UTF-8 is clearly recognizable, widely used,
--   and people may have UTF-8 patches even when UTF-8 is not their locale.
metadataToString :: B.ByteString -> String
metadataToString bs | '\xfffd' `notElem` bsUtf8 = bsUtf8
                    | otherwise                 = decodeLocale bs
  where bsUtf8 = unpackPSFromUTF8 bs

friendlyD :: B.ByteString -> String
friendlyD d = unsafePerformIO $ do
    ct <- toCalendarTime $ toClockTime $ readPatchDate d
    return $ calendarTimeToString ct

toXml :: PatchInfo -> Doc
toXml = toXml' True

toXmlShort :: PatchInfo -> Doc
toXmlShort = toXml' False

toXml' :: Bool -> PatchInfo -> Doc
toXml' includeComments pi =
        text "<patch"
    <+> text "author='" <> escapeXMLByteString (_piAuthor pi) <> text "'"
    <+> text "date='" <> escapeXMLByteString (_piDate pi) <> text "'"
    <+> text "local_date='" <> escapeXML (friendlyD $ _piDate pi) <> text "'"
    <+> text "inverted='" <> text (show $ _piLegacyIsInverted pi) <> text "'"
    <+> text "hash='" <> text (show $ makePatchname pi) <> text "'>"
    $$  indent abstract
    $$  text "</patch>"
      where
        indent = prefix "    "
        name = text "<name>" <> escapeXMLByteString (_piName pi) <> text "</name>"
        abstract | includeComments = name $$ commentsAsXml (_piLog pi)
                 | otherwise = name

commentsAsXml :: [B.ByteString] -> Doc
commentsAsXml comments
  | B.length comments' > 0 = text "<comment>"
                          <> escapeXMLByteString comments'
                          <> text "</comment>"
  | otherwise = empty
    where comments' = unlinesPS comments

-- escapeXML is duplicated in Patch.lhs and Annotate.lhs
-- It should probably be refactored to exist in one place.
escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

-- Escape XML characters in a UTF-8 encoded ByteString, and turn it into a Doc.
-- The data will be in the Doc as a bytestring.
escapeXMLByteString :: B.ByteString -> Doc
escapeXMLByteString = packedString . bstrReplace '\'' "&apos;"
                                   . bstrReplace '"'  "&quot;"
                                   . bstrReplace '>'  "&gt;"
                                   . bstrReplace '<'  "&lt;"
                                   . bstrReplace '&'  "&amp;"

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ strReplace x y zs
  | otherwise = z : strReplace x y zs

bstrReplace :: Char -> String -> B.ByteString -> B.ByteString
bstrReplace c s bs | B.null bs   = B.empty
                   | otherwise   = if BC.head bs == c
                                     then B.append (BC.pack s)
                                                   (bstrReplace c s (B.tail bs))
                                     else B.cons (B.head bs)
                                                 (bstrReplace c s (B.tail bs))

-- | Hash on patch metadata (patch name, author, date, log, and the legacy
-- \"inverted\" flag.
-- Robust against context changes but does not guarantee patch contents.
-- Usually used as matcher or patch identifier (see Darcs.Patch.Match).
makePatchname :: PatchInfo -> SHA1
makePatchname pi = sha1PS sha1_me
        where b2ps True = BC.pack "t"
              b2ps False = BC.pack "f"
              sha1_me = B.concat [_piName pi,
                                  _piAuthor pi,
                                  _piDate pi,
                                  B.concat $ _piLog pi,
                                  b2ps $ _piLegacyIsInverted pi]


showPatchInfo :: ShowPatchFor -> PatchInfo -> Doc
showPatchInfo ForDisplay = displayPatchInfo
showPatchInfo ForStorage = storePatchInfo

-- |Patch is stored between square brackets.
--
-- > [ <patch name>
-- > <patch author>*<patch date>
-- >  <patch log (may be empty)> (indented one)
-- >  <can have multiple lines in patch log,>
-- >  <as long as they're preceded by a space>
-- >  <and don't end with a square bracket.>
-- > ]
--
-- note that below I assume the name has no newline in it.
-- See 'readPatchInfo' for the inverse operation.
-- There are more assumptions, see validation functions above.
storePatchInfo :: PatchInfo -> Doc
storePatchInfo pi =
    blueText "[" <> packedString (_piName pi)
 $$ packedString (_piAuthor pi) <> text inverted <> packedString (_piDate pi)
                                 <> myunlines (_piLog pi) <> blueText "] "
    where inverted = if _piLegacyIsInverted pi then "*-" else "**"
          myunlines [] = empty
          myunlines xs =
              foldr (\s -> ((text "\n " <> packedString s) <>)) (text "\n") xs

-- |Parser for 'PatchInfo' as stored in patch bundles and inventory files,
-- for example:
--
-- > [Document the foo interface
-- > John Doe <john.doe@example.com>**20110615084241
-- >  Ignore-this: 85b94f67d377c4ab671101266ef9c229
-- >  Nobody knows what a 'foo' is, so describe it.
-- > ]
--
-- See 'showPatchInfo' for the inverse operation.
readPatchInfo :: Parser PatchInfo
readPatchInfo = do
  skipSpace
  char '['
  name <- takeTillChar '\n'
  _ <- anyChar
  author <- takeTillChar '*'
  s2 <- RM.take 2
  ct <- takeTill (\c->c==']'||c=='\n')
  option () (void (char '\n')) -- consume newline char, if present
  log <- linesStartingWithEndingWith ' ' ']'
  return PatchInfo { _piDate = ct
                   , _piName = name
                   , _piAuthor = author
                   , _piLog = log
                   , _piLegacyIsInverted = BC.index s2 1 /= '*'
                   }
