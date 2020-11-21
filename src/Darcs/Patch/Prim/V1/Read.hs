{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Read () where

import Darcs.Prelude

import Darcs.Patch.Prim.Class ( PrimRead(..), hunk, binary )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..)
    , DirPatchType(..)
    , FilePatchType(..)
    )

import Darcs.Util.Path (  )
import Darcs.Patch.Format ( FileNameFormat )
import Darcs.Patch.Read ( readFileName )
import Darcs.Util.Parser
    ( Parser, takeTillChar, string, int
    , option, choice, anyChar, char, lexWord
    , skipSpace, skipWhile, linesStartingWith
    )

import Darcs.Patch.Witnesses.Sealed ( seal )

import Darcs.Util.ByteString ( fromHex2PS )

import Control.Monad ( liftM )
import qualified Data.ByteString       as B  ( ByteString, init, tail, concat )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )


instance PrimRead Prim where
  readPrim fmt
     = skipSpace >> choice
       [ return' $ readHunk fmt
       , return' $ readAddFile fmt
       , return' $ readAddDir fmt
       , return' $ readMove fmt
       , return' $ readRmFile fmt
       , return' $ readRmDir fmt
       , return' $ readTok fmt
       , return' $ readBinary fmt
       , return' readChangePref
       ]
    where
    return'  = liftM seal

hunk' :: B.ByteString
hunk' = BC.pack "hunk"

replace :: B.ByteString
replace = BC.pack "replace"

binary' :: B.ByteString
binary' = BC.pack "binary"

addfile :: B.ByteString
addfile = BC.pack "addfile"

adddir :: B.ByteString
adddir = BC.pack "adddir"

rmfile :: B.ByteString
rmfile = BC.pack "rmfile"

rmdir :: B.ByteString
rmdir = BC.pack "rmdir"

move :: B.ByteString
move = BC.pack "move"

changepref :: B.ByteString
changepref = BC.pack "changepref"

readHunk :: FileNameFormat -> Parser (Prim wX wY)
readHunk fmt = do
  string hunk'
  fi <- readFileName fmt
  l <- int
  have_nl <- skipNewline
  if have_nl
    then do
      _ <- linesStartingWith ' ' -- skipping context
      old <- linesStartingWith '-'
      new <- linesStartingWith '+'
      _ <- linesStartingWith ' ' -- skipping context
      return $ hunk fi l old new
    else return $ hunk fi l [] []

skipNewline :: Parser Bool
skipNewline = option False (char '\n' >> return True)

readTok :: FileNameFormat -> Parser (Prim wX wY)
readTok fmt = do
  string replace
  f <- readFileName fmt
  regstr <- lexWord
  o <- lexWord
  n <- lexWord
  return $ FP f $ TokReplace (BC.unpack (drop_brackets regstr))
                             (BC.unpack o) (BC.unpack n)
    where drop_brackets = B.init . B.tail


-- * Binary file modification
--
-- | Modify a binary file
--
-- > binary FILENAME
-- > oldhex
-- > *HEXHEXHEX
-- > ...
-- > newhex
-- > *HEXHEXHEX
-- > ...
readBinary :: FileNameFormat -> Parser (Prim wX wY)
readBinary fmt = do
  string binary'
  fi <- readFileName fmt
  _ <- lexWord
  skipSpace
  old <- linesStartingWith '*'
  _ <- lexWord
  skipSpace
  new <- linesStartingWith '*'
  return $ binary fi (fromHex2PS $ B.concat old) (fromHex2PS $ B.concat new)

readAddFile :: FileNameFormat -> Parser (Prim wX wY)
readAddFile fmt = do
  string addfile
  f <- readFileName fmt
  return $ FP f AddFile

readRmFile :: FileNameFormat -> Parser (Prim wX wY)
readRmFile fmt = do
  string rmfile
  f <- readFileName fmt
  return $ FP f RmFile

readMove :: FileNameFormat -> Parser (Prim wX wY)
readMove fmt = do
  string move
  d <- readFileName fmt
  d' <- readFileName fmt
  return $ Move d d'

readChangePref :: Parser (Prim wX wY)
readChangePref = do
  string changepref
  p <- lexWord
  skipWhile (== ' ')
  _ <- anyChar -- skip newline
  f <- takeTillChar '\n'
  _ <- anyChar -- skip newline
  t <- takeTillChar '\n'
  return $ ChangePref (BC.unpack p) (BC.unpack f) (BC.unpack t)

readAddDir :: FileNameFormat -> Parser (Prim wX wY)
readAddDir fmt = do
  string adddir
  f <- readFileName fmt
  return $ DP f AddDir

readRmDir :: FileNameFormat -> Parser (Prim wX wY)
readRmDir fmt = do
  string rmdir
  f <- readFileName fmt
  return $ DP f RmDir
