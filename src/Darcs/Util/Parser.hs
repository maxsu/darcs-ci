module Darcs.Util.Parser
    ( Parser
    , anyChar
    , char
    , checkConsumes
    , choice
    , endOfInput
    , int
    , lexChar
    , lexString
    , linesStartingWith
    , linesStartingWithEndingWith
    , lexWord
    , option
    , optional
    , parse
    , skipSpace
    , skipWhile
    , string
    , take
    , takeTill
    , takeTillChar
    ) where

import Control.Applicative ( empty, many, optional, (<|>) )

import Darcs.Prelude hiding ( lex, take )

import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 hiding ( parse, char, string )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B

parse :: Parser a -> B.ByteString -> Either String (a, B.ByteString)
parse p bs =
  case AC.parse p bs of
    Fail _ ss s -> Left $ unlines (s:ss)
    Partial k ->
      case k B.empty of
        Fail _ ss s -> Left $ unlines (s:ss)
        Partial _ -> error "impossible"
        Done i r -> Right (r, i)
    Done i r -> Right (r, i)

{-# INLINE skip #-}
skip :: Parser a -> Parser ()
skip p = p >> return ()

{-# INLINE lex #-}
lex :: Parser a -> Parser a
lex p = skipSpace >> p

{-# INLINE lexWord #-}
lexWord :: Parser B.ByteString
lexWord = lex (A.takeWhile1 (not . isSpace_w8))

{-# INLINE lexChar #-}
lexChar :: Char -> Parser ()
lexChar c = lex (char c)

{-# inline lexString #-}
lexString :: B.ByteString -> Parser ()
lexString s = lex (string s)

{-# INLINE char #-}
char :: Char -> Parser ()
char = skip . AC.char

{-# INLINE string #-}
string :: B.ByteString -> Parser ()
string = skip . AC.string

{-# INLINE int #-}
int :: Parser Int
int = lex (signed decimal)

{-# INLINE takeTillChar #-}
takeTillChar :: Char -> Parser B.ByteString
takeTillChar c = takeTill (== c)

{-# INLINE checkConsumes #-}
checkConsumes :: Parser a -> Parser a
checkConsumes parser = do
  (consumed, result) <- match parser
  if B.null consumed
    then empty
    else return result

{-# INLINE linesStartingWith #-}
linesStartingWith :: Char -> Parser [B.ByteString]
linesStartingWith c = many $ do
  char c
  r <- takeTillChar '\n'
  skip (char '\n') <|> endOfInput
  return r

{-# INLINE linesStartingWithEndingWith #-}
linesStartingWithEndingWith :: Char -> Char -> Parser [B.ByteString]
linesStartingWithEndingWith st en = do
  ls <- linesStartingWith st
  char en
  return ls
