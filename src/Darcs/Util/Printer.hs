-- | Darcs pretty printing library
--
-- The combinator names are taken from 'Text.PrettyPrint.HughesPJ', although
-- the behaviour of the two libraries is slightly different.
--
-- This code was made generic in the element type by Juliusz Chroboczek.
module Darcs.Util.Printer
    (
    -- * 'Doc' type and structural combinators
      Doc(Doc,unDoc)
    , empty, (<>), (<?>), (<+>), ($$), ($+$), vcat, vsep, hcat, hsep
    , minus, newline, plus, space, backslash, lparen, rparen
    , parens, sentence
    -- * Constructing 'Doc's
    , text
    , hiddenText
    , invisibleText
    , wrapText, quoted
    , formatText
    , formatWords
    , pathlist
    , userchunk, packedString
    , prefix
    , hiddenPrefix
    , insertBeforeLastline
    , prefixLines
    , invisiblePS, userchunkPS
    -- * Rendering to 'String'
    , renderString, renderStringWith
    -- * Rendering to 'ByteString'
    , renderPS, renderPSWith
    , renderPSs, renderPSsWith
    -- * Printers
    , Printers
    , Printers'(..)
    , Printer
    , simplePrinters, invisiblePrinter, simplePrinter
    -- * Printables
    , Printable(..)
    , doc
    , printable, invisiblePrintable, hiddenPrintable, userchunkPrintable
    -- * Constructing colored 'Doc's
    , Color(..)
    , blueText, redText, greenText, magentaText, cyanText
    , colorText
    , lineColor
    -- * IO, uses 'Data.ByteString.hPut' for output
    , hPutDoc,     hPutDocLn,     putDoc,     putDocLn
    , hPutDocWith, hPutDocLnWith, putDocWith, putDocLnWith
    , hPutDocCompr
    , debugDocLn
    -- * TODO: It is unclear what is unsafe about these constructors
    , unsafeText, unsafeBoth, unsafeBothText, unsafeChar
    , unsafePackedString
    ) where

import Darcs.Prelude

import Data.String ( IsString(..) )
import System.IO ( Handle, stdout )
import qualified Data.ByteString as B ( ByteString, hPut, concat )
import qualified Data.ByteString.Char8 as BC ( singleton )

import Darcs.Util.ByteString ( linesPS, decodeLocale, encodeLocale, gzWriteHandle )
import Darcs.Util.Global ( debugMessage )

-- | A 'Printable' is either a String, a packed string, or a chunk of
-- text with both representations.
data Printable = S !String
               | PS !B.ByteString
               | Both !String !B.ByteString

-- | 'Printable' representation of a space
spaceP :: Printable
spaceP   = Both " "  (BC.singleton ' ')

-- | 'Printable' representation of a newline.
newlineP :: Printable
newlineP = S "\n"

-- | A 'Doc' representing a space (\" \")
space :: Doc
space = unsafeBoth " "  (BC.singleton ' ')

-- | A 'Doc' representing a newline
newline :: Doc
newline = unsafeChar '\n'

-- | A 'Doc' representing a \"-\"
minus :: Doc
minus = unsafeBoth "-"  (BC.singleton '-')

-- | A 'Doc' representing a \"+\"
plus :: Doc
plus = unsafeBoth "+"  (BC.singleton '+')

-- | A 'Doc' representing a \"\\\"
backslash :: Doc
backslash = unsafeBoth "\\" (BC.singleton '\\')

-- | A 'Doc' that represents @\"(\"@
lparen :: Doc
lparen = unsafeBoth  "(" (BC.singleton '(')

-- | A 'Doc' that represents @\")\"@
rparen :: Doc
rparen = unsafeBoth ")" (BC.singleton ')')

-- | prop> parens d = lparen <> d <> rparen
parens :: Doc -> Doc
parens d = lparen <> d <> rparen

-- | Turn a 'Doc' into a sentence. This appends a ".".
sentence :: Doc -> Doc
sentence = (<> text ".")

-- | Format a list of 'FilePath's as quoted text. It deliberately refuses to
-- use English.andClauses but rather separates the quoted strings only with a
-- space, because this makes it usable for copy and paste e.g. as arguments to
-- another shell command.
pathlist :: [FilePath] -> Doc
pathlist paths = hsep (map quoted paths)

-- | 'putDocWith' puts a 'Doc' on stdout using the given printer.
putDocWith :: Printers -> Doc -> IO ()
putDocWith prs = hPutDocWith prs stdout

-- | 'putDocLnWith' puts a 'Doc', followed by a newline on stdout using
-- the given printer.
putDocLnWith :: Printers -> Doc -> IO ()
putDocLnWith prs = hPutDocLnWith prs stdout

-- | 'putDoc' puts a 'Doc' on stdout using the simple printer 'simplePrinters'.
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | 'putDocLn' puts a 'Doc', followed by a newline on stdout using
-- 'simplePrinters'
putDocLn :: Doc -> IO ()
putDocLn = hPutDocLn stdout

-- | 'hputDocWith' puts a 'Doc' on the given handle using the given printer.
hPutDocWith :: Printers -> Handle -> Doc -> IO ()
hPutDocWith prs h d = do
  p <- prs h
  hPrintPrintables h (renderWith p d)

-- | 'hputDocLnWith' puts a 'Doc', followed by a newline on the given
-- handle using the given printer.
hPutDocLnWith :: Printers -> Handle -> Doc -> IO ()
hPutDocLnWith prs h d = hPutDocWith prs h (d <?> newline)

-- |'hputDoc' puts a 'Doc' on the given handle using 'simplePrinters'
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc = hPutDocWith simplePrinters

-- | 'hputDocLn' puts a 'Doc', followed by a newline on the given handle using
-- 'simplePrinters'.
hPutDocLn :: Handle -> Doc -> IO ()
hPutDocLn = hPutDocLnWith simplePrinters

-- | like 'hPutDoc' but with compress data before writing
hPutDocCompr :: Handle -> Doc -> IO ()
hPutDocCompr h = gzWriteHandle h . renderPSs

-- | Write a 'Doc' to stderr if debugging is turned on.
debugDocLn :: Doc -> IO ()
debugDocLn = debugMessage . renderString

-- | @'hPrintPrintables' h@ prints a list of 'Printable's to the handle @h@
-- It uses binary output of 'ByteString's. If these not available,
-- converts according to locale.
hPrintPrintables :: Handle -> [Printable] -> IO ()
hPrintPrintables h = mapM_ (hPrintPrintable h)

-- | @'hPrintPrintable' h@ prints a 'Printable' to the handle @h@.
hPrintPrintable :: Handle -> Printable -> IO ()
hPrintPrintable h (S ps) = B.hPut h (encodeLocale ps)
hPrintPrintable h (PS ps) = B.hPut h ps
hPrintPrintable h (Both _ ps) = B.hPut h ps

-- | A 'Doc' is a bit of enriched text. 'Doc's are concatenated using
-- '<>' from class 'Monoid', which is right-associative.
newtype Doc = Doc { unDoc :: St -> Document }

-- | Together with the language extension OverloadedStrings, this allows to
-- use string literals where a 'Doc' is expected.
instance IsString Doc where
   fromString = text

-- | The State associated with a 'Doc'. Contains a set of printers for each
-- hanlde, and the current prefix of the document.
data St = St { printers :: !Printers',
               currentPrefix :: !([Printable] -> [Printable]) }
type Printers = Handle -> IO Printers'

-- | A set of printers to print different types of text to a handle.
data Printers' = Printers {colorP :: !(Color -> Printer),
                           invisibleP :: !Printer,
                           hiddenP :: !Printer,
                           userchunkP :: !Printer,
                           defP :: !Printer,
                           lineColorT :: !(Color -> Doc -> Doc),
                           lineColorS :: !([Printable] -> [Printable])
                          }
type Printer = Printable -> St -> Document

data Color = Blue | Red | Green | Cyan | Magenta

-- | 'Document' is a wrapper around '[Printable] -> [Printable]' which allows
-- to handle the special case of an empty 'Document' in a non-uniform manner.
-- The simplest 'Documents' are built from 'String's using 'text'.
data Document = Document ([Printable] -> [Printable])
              | Empty

-- | renders a 'Doc' into a 'String' with control codes for the
-- special features of the 'Doc'.
renderString :: Doc -> String
renderString = renderStringWith simplePrinters'

-- | renders a 'Doc' into a 'String' using a given set of printers.
-- If content is only available as 'ByteString', decode according to
-- the current locale.
renderStringWith :: Printers' -> Doc -> String
renderStringWith prs d = concatMap (toString) $ renderWith prs d
    where toString (S s) = s
          toString (PS ps) = decodeLocale ps
          toString (Both s _) = s

-- | renders a 'Doc' into 'B.ByteString' with control codes for the
-- special features of the Doc. See also 'readerString'.
renderPS :: Doc -> B.ByteString
renderPS = renderPSWith simplePrinters'

-- | renders a 'Doc' into a list of 'PackedStrings', one for each line.
renderPSs :: Doc -> [B.ByteString]
renderPSs = renderPSsWith simplePrinters'

-- | renders a 'Doc' into a 'B.ByteString' using a given set of printers.
renderPSWith :: Printers' -> Doc -> B.ByteString
renderPSWith prs d = B.concat $ renderPSsWith prs d

-- | renders a 'Doc' into a list of 'PackedStrings', one for each
-- chunk of text that was added to the 'Doc', using the given set of
-- printers.
renderPSsWith :: Printers' -> Doc -> [B.ByteString]
renderPSsWith prs d = map toPS $ renderWith prs d
    where toPS (S s)        = encodeLocale s
          toPS (PS ps)      = ps
          toPS (Both _ ps)  = ps

-- | renders a 'Doc' into a list of 'Printables' using a set of
-- printers. Each item of the list corresponds to a string that was
-- added to the 'Doc'.
renderWith :: Printers' -> Doc -> [Printable]
renderWith ps (Doc d) = case d (initState ps) of
                        Empty -> []
                        Document f -> f []

initState :: Printers' -> St
initState prs = St { printers = prs, currentPrefix = id }

prefix :: String -> Doc -> Doc
prefix s (Doc d) = Doc $ \st ->
                   let p = S s
                       st' = st { currentPrefix = currentPrefix st . (p:) } in
                   case d st' of
                     Document d'' -> Document $ (p:) . d''
                     Empty -> Empty

-- TODO try to find another way to do this, it's rather a violation
-- of the Doc abstraction
prefixLines :: Doc -> Doc -> Doc
prefixLines prefixer prefixee =
  vcat $ map (prefixer <+>) $ map packedString $ linesPS $ renderPS prefixee

-- TODO try to find another way to do this, it's rather a violation
-- of the Doc abstraction
insertBeforeLastline :: Doc -> Doc -> Doc
insertBeforeLastline a b =
  case reverse $ map packedString $ linesPS $ renderPS a of
    (ll:ls) -> vcat (reverse ls) $$ b $$ ll
    [] ->
      error "empty Doc given as first argument of Printer.insert_before_last_line"

lineColor :: Color -> Doc -> Doc
lineColor c d = Doc $ \st -> case lineColorT (printers st) c d of
                             Doc d' -> d' st

hiddenPrefix :: String -> Doc -> Doc
hiddenPrefix s (Doc d) =
    Doc $ \st -> let pr = printers st
                     p = S (renderStringWith pr $ hiddenText s)
                     st' = st { currentPrefix = currentPrefix st . (p:) }
                 in case d st' of
                      Document d'' -> Document $ (p:) . d''
                      Empty -> Empty

-- | 'unsafeBoth' builds a Doc from a 'String' and a 'B.ByteString' representing
-- the same text, but does not check that they do.
unsafeBoth :: String -> B.ByteString -> Doc
unsafeBoth s ps = Doc $ simplePrinter (Both s ps)

-- | 'unsafeBothText' builds a 'Doc' from a 'String'. The string is stored in the
-- Doc as both a String and a 'B.ByteString'.
unsafeBothText :: String -> Doc
unsafeBothText s = Doc $ simplePrinter (Both s (encodeLocale s))

-- | 'packedString' builds a 'Doc' from a 'B.ByteString' using 'printable'
packedString :: B.ByteString -> Doc
packedString = printable . PS

-- | 'unsafePackedString' builds a 'Doc' from a 'B.ByteString' using 'simplePrinter'
unsafePackedString :: B.ByteString -> Doc
unsafePackedString = Doc . simplePrinter . PS

-- | 'invisiblePS' creates a 'Doc' with invisible text from a 'B.ByteString'
invisiblePS :: B.ByteString -> Doc
invisiblePS = invisiblePrintable . PS

-- | 'userchunkPS' creates a 'Doc' representing a user chunk from a 'B.ByteString'.
--
-- Rrrright. And what, please is that supposed to mean?
userchunkPS :: B.ByteString -> Doc
userchunkPS = userchunkPrintable . PS

-- | 'unsafeChar' creates a Doc containing just one character.
unsafeChar :: Char -> Doc
unsafeChar = unsafeText . (:"")

-- | 'text' creates a 'Doc' from a @String@, using 'printable'.
text :: String -> Doc
text = printable . S

-- | 'unsafeText' creates a 'Doc' from a 'String', using 'simplePrinter' directly
unsafeText :: String -> Doc
unsafeText = Doc . simplePrinter . S

-- | 'invisibleText' creates a 'Doc' containing invisible text from a @String@
invisibleText :: String -> Doc
invisibleText = invisiblePrintable . S

-- | 'hiddenText' creates a 'Doc' containing hidden text from a @String@
hiddenText :: String -> Doc
hiddenText = hiddenPrintable . S

-- | 'userchunk' creates a 'Doc' containing a user chunk from a @String@
userchunk :: String -> Doc
userchunk = userchunkPrintable . S

blueText, redText, greenText, magentaText, cyanText :: String -> Doc
blueText = colorText Blue
redText = colorText Red
greenText = colorText Green
magentaText = colorText Magenta
cyanText = colorText Cyan

-- | 'colorText' creates a 'Doc' containing colored text from a @String@
colorText :: Color -> String -> Doc
colorText c = mkColorPrintable c . S

-- | @'wrapText' n s@ is a 'Doc' representing @s@ line-wrapped at 'n' characters
wrapText :: Int -> String -> Doc
wrapText n s =
    vcat . map text . reverse $ foldl add_to_line [] (words s)
  where add_to_line [] a = [a]
        add_to_line ("":d) a = a:d
        add_to_line (l:ls) new | length l + length new > n = new:l:ls
        add_to_line (l:ls) new = (l ++ " " ++ new):ls

-- | Given a list of 'String's representing the words of a paragraph, format
-- the paragraphs using 'wrapText' and separate them with an empty line.
formatText :: Int -> [String] -> Doc
formatText w = vsep . map (wrapText w)

-- | A variant of 'wrapText' that takes a list of strings as input.
-- Useful when @{-# LANGUAGE CPP #-}@ makes it impossible to use multiline
-- string literals.
formatWords :: [String] -> Doc
formatWords = wrapText 80 . unwords

-- | Creates a 'Doc' from any 'Printable'.
printable :: Printable -> Doc
printable x = Doc $ \st -> defP (printers st) x st

mkColorPrintable :: Color -> Printable -> Doc
mkColorPrintable c x = Doc $ \st -> colorP (printers st) c x st

-- | Creates an invisible 'Doc' from any 'Printable'.
invisiblePrintable :: Printable -> Doc
invisiblePrintable x = Doc $ \st -> invisibleP (printers st) x st

-- | Creates a hidden 'Doc' from any 'Printable'.
hiddenPrintable :: Printable -> Doc
hiddenPrintable x = Doc $ \st -> hiddenP (printers st) x st

-- | Creates... WTF is a userchunk???
userchunkPrintable :: Printable -> Doc
userchunkPrintable x = Doc $ \st -> userchunkP (printers st) x st

-- | 'simplePrinters' is a 'Printers' which uses the set 'simplePriners\'' on any
-- handle.
simplePrinters :: Printers
simplePrinters _ = return simplePrinters'

-- | A set of default printers suitable for any handle. Does not use color.
simplePrinters' :: Printers'
simplePrinters'  = Printers { colorP = const simplePrinter,
                              invisibleP = simplePrinter,
                              hiddenP = invisiblePrinter,
                              userchunkP = simplePrinter,
                              defP = simplePrinter,
                              lineColorT = const id,
                              lineColorS = id
                            }

-- | 'simplePrinter' is the simplest 'Printer': it just concatenates together
-- the pieces of the 'Doc'
simplePrinter :: Printer
simplePrinter x = unDoc $ doc (\s -> x:s)

-- | 'invisiblePrinter' is the 'Printer' for hidden text. It just replaces
-- the document with 'empty'.  It's useful to have a printer that doesn't
-- actually do anything because this allows you to have tunable policies,
-- for example, only printing some text if it's to the terminal, but not
-- if it's to a file or vice-versa.
invisiblePrinter :: Printer
invisiblePrinter _ = unDoc empty

infixr 6 `append`
infixr 6 <+>
infixr 5 $+$
infixr 5 $$

-- | The empty 'Doc'
empty :: Doc
empty = Doc $ const Empty

doc :: ([Printable] -> [Printable]) -> Doc
doc f = Doc $ const $ Document f

instance Semigroup Doc where
  (<>) = append

-- | 'mappend' ('<>') is concatenation, 'mempty' is the 'empty' 'Doc'
instance Monoid Doc where
  mempty = empty
  mappend = append

-- | Concatenation of two 'Doc's
append :: Doc -> Doc -> Doc
Doc a `append` Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> bf s)

-- | @a '<?>' b@ is @a '<>' b@ if @a@ is not empty, else empty
(<?>) :: Doc -> Doc -> Doc
Doc a <?> Doc b =
    Doc $ \st -> case a st of
                 Empty -> Empty
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf -> bf s)

-- | @a '<+>' b@ is @a@ followed by @b@ with a space in between if both are non-empty
(<+>) :: Doc -> Doc -> Doc
Doc a <+> Doc b =
    Doc $ \st -> case a st of
                 Empty -> b st
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf ->
                                                         spaceP:bf s)

-- | @a '$$' b@ is @a@ above @b@
($$) :: Doc -> Doc -> Doc
Doc a $$ Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> sf (newlineP:pf (bf s)))
                        where pf = currentPrefix st
                              sf = lineColorS $ printers st

-- | @a '$+$' b@ is @a@ above @b@ with an empty line in between if both are non-empty
($+$) :: Doc -> Doc -> Doc
Doc a $+$ Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> sf (newlineP:newlineP:pf (bf s)))
                        where pf = currentPrefix st
                              sf = lineColorS $ printers st

-- | Pile 'Doc's vertically
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- | Pile 'Doc's vertically, with a blank line in between
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | Concatenate 'Doc's horizontally
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate 'Doc's horizontally with a space as separator
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

-- | Quote a string for screen output
quoted :: String -> Doc
quoted s = text "\"" <> text (escape s) <> text "\""
  where
    escape "" = ""
    escape (c:cs) = if c `elem` ['\\', '"']
                       then '\\' : c : escape cs
                       else c : escape cs
