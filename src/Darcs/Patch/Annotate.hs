{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 Petr Rockai
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |
-- Module      : Darcs.Patch.Annotate
-- Copyright   : 2010 Petr Rockai
-- License     : MIT
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Annotate
    (
      annotateFile
    , annotateDirectory
    , format
    , machineFormat
    , AnnotateResult
    , Annotate(..)
    , AnnotateRP
    ) where

import Darcs.Prelude

import Control.Monad.State ( modify, modify', when, gets, State, execState )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Function ( on )
import Data.List( nub, groupBy )
import Data.Maybe( isJust, mapMaybe )

import qualified Darcs.Patch.Prim.FileUUID as FileUUID

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FromPrim ( PrimOf(..) )
import Darcs.Patch.Info ( PatchInfo(..), displayPatchInfo, piAuthor, makePatchname )
import Darcs.Patch.Invert ( Invert, invert )
import Darcs.Patch.Named ( patchcontents )
import Darcs.Patch.PatchInfoAnd( info, PatchInfoAnd, hopefully )
import Darcs.Patch.Prim.V1.Core ( Prim(..), DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.TokenReplace ( annotateReplace )
import Darcs.Patch.Witnesses.Ordered

import Darcs.Util.Path ( AnchoredPath, movedirfilename, flatten )
import Darcs.Util.Printer( renderString )
import Darcs.Util.ByteString ( linesPS, decodeLocale )

data FileOrDirectory = File
                     | Directory
                       deriving (Show, Eq)

type AnnotateResult = V.Vector (Maybe PatchInfo, B.ByteString)

data Content2 f g
  = FileContent (f (g B.ByteString))
  | DirContent (f (g AnchoredPath))

data Annotated2 f g = Annotated2
    { annotated     :: !AnnotateResult
    , current       :: !(Content2 f g)
    , currentPath   :: (Maybe AnchoredPath)
    , currentInfo   :: PatchInfo
    }

type Content = Content2 [] ((,) Int)
type Annotated = Annotated2 [] ((,) Int)

deriving instance Eq Content
deriving instance Show Content

deriving instance Eq Annotated
deriving instance Show Annotated

type AnnotatedM = State Annotated

class Annotate p where
  annotate :: p wX wY -> AnnotatedM ()

-- |This constraint expresses what is needed for a repo patch to
-- support the high-level interface to annotation
-- (currently annotateFile and annotateDirectory)
type AnnotateRP p = (Annotate (PrimOf p), Invert (PrimOf p), Effect p)

instance Annotate Prim where
  annotate (FP fn fp) = case fp of
    RmFile -> do
      whenPathIs fn $ modify' (\s -> s { currentPath = Nothing })
      withDirectory $ updateDirectory fn
    AddFile -> return ()
    Hunk off o n -> whenPathIs fn $ withFile $ \c -> do
      let remove = length o
      let add = length n
      i <- gets currentInfo
      a <- gets annotated
      -- NOTE patches are inverted and in inverse order
      modify' $ \s ->
        -- NOTE subtract one from offset because darcs counts from one,
        -- whereas vectors and lists count from zero.
        let (to,from) = splitAt (off-1) c
        in  s { current = FileContent $ map eval $ to ++ replicate add (-1, B.empty) ++ drop remove from
              , annotated = merge i a $ map eval $ take remove $ from
              }
    TokReplace t o n -> whenPathIs fn $ withFile $ \c -> do
      let test = annotateReplace t (BC.pack o) (BC.pack n)
      i <- gets currentInfo
      a <- gets annotated
      modify' $ \s -> s
        { current = FileContent $ map (\(ix,b)->if test b then (-1,B.empty) else (ix,b)) c
        , annotated = merge i a $ map eval $ filter (test . snd) $ c
        }
    -- TODO what if the status of a file changed from text to binary?
    Binary _ _ -> whenPathIs fn $ error "annotate: can't handle binary changes"
  annotate (DP _ AddDir) = return ()
  annotate (DP fn RmDir) = withDirectory $ \c -> do
    whenPathIs fn $ modify' (\s -> s { currentPath = Nothing })
    updateDirectory fn c
  annotate (Move fn fn') = do
    modify' (\s -> s { currentPath = fmap (movedirfilename fn fn') (currentPath s) })
    withDirectory $ \c -> do
      let fix (i, x) = (i, movedirfilename fn fn' x)
      modify $ \s -> s { current = DirContent $ map fix c }
  annotate (ChangePref _ _ _) = return ()

instance Annotate FileUUID.Prim where
  annotate _ = error "annotate not implemented for FileUUID patches"

annotatePIAP :: AnnotateRP p => PatchInfoAnd rt p wX wY -> AnnotatedM ()
annotatePIAP =
  sequence_ . mapFL annotate . invert . effect . patchcontents . hopefully

withDirectory :: ([(Int, AnchoredPath)] -> AnnotatedM ()) -> AnnotatedM ()
withDirectory actions = do
  what <- gets current
  case what of
    DirContent c -> actions c
    FileContent _ -> return ()

withFile :: ([(Int, B.ByteString)] -> AnnotatedM ()) -> AnnotatedM ()
withFile actions = do
  what <- gets current
  case what of
    FileContent c -> actions c
    DirContent _ -> return ()

whenPathIs :: AnchoredPath -> AnnotatedM () -> AnnotatedM ()
whenPathIs fn actions = do
  p <- gets currentPath
  when (p == Just fn) actions

eval :: (Int, a) -> (Int, a)
eval (i,b) = seq i $ seq b $ (i,b)

merge :: a
      -> V.Vector (Maybe a, BC.ByteString)
      -> [(Int, t)]
      -> V.Vector (Maybe a, BC.ByteString)
merge i a l = a V.// [ (line, (Just i, B.empty))
                     | (line, _) <- l, line >= 0 && line < V.length a]

updateDirectory :: AnchoredPath -> [(Int,AnchoredPath)] -> AnnotatedM ()
updateDirectory path files = do
    case filter ((==path) . snd) files of
      [match@(ident, _)] -> reannotate ident match
      _ -> return ()
  where
    reannotate :: Int -> (Int, AnchoredPath) -> AnnotatedM ()
    reannotate ident match =
      modify $ \x -> x { annotated = annotated x V.// [ (ident, update $ currentInfo x) ]
                       , current = DirContent $ filter (/= match) files }
    update inf = (Just inf, flatten path)

complete :: Annotated -> Bool
complete x = V.all (isJust . fst) $ annotated x

annotate' :: AnnotateRP p
          => RL (PatchInfoAnd rt p) wX wY
          -> Annotated
          -> Annotated
annotate' NilRL ann = ann
annotate' (ps :<: p) ann
    | complete ann = ann
    | otherwise = annotate' ps $ execState (annotatePIAP p) (ann { currentInfo = info p })

annotateFile :: AnnotateRP p
             => RL (PatchInfoAnd rt p) wX wY
             -> AnchoredPath
             -> B.ByteString
             -> AnnotateResult
annotateFile patches inipath inicontent = annotated $ annotate' patches initial
  where
    initial = Annotated2 { currentPath = Just inipath
                        , currentInfo = error "There is no currentInfo."
                        , current = FileContent $ zip [0..] (linesPS inicontent)
                        , annotated = V.replicate (length $ breakLines inicontent)
                                                      (Nothing, B.empty)
                        }

annotateDirectory :: AnnotateRP p
                  => RL (PatchInfoAnd rt p) wX wY
                  -> AnchoredPath
                  -> [AnchoredPath]
                  -> AnnotateResult
annotateDirectory patches inipath inicontent = annotated $ annotate' patches initial
  where
    initial = Annotated2 { currentPath = Just inipath
                        , currentInfo = error "There is no currentInfo."
                        , current = DirContent $ zip [0..] inicontent
                        , annotated = V.replicate (length inicontent) (Nothing, B.empty)
                        }

machineFormat :: B.ByteString -> AnnotateResult -> String
machineFormat d a = unlines [ case i of
                                 Just inf -> show $ makePatchname inf
                                 Nothing -> -- make unknowns uniform, for easier parsing
                                   take 40 ( repeat '0' ) -- fake hash of the right size
                              ++ " | " ++ BC.unpack line ++ " " ++ BC.unpack add
                            | ((i, add), line) <- zip (V.toList a) (breakLines d) ]

format :: B.ByteString -> AnnotateResult -> String
format d a = pi_list ++ "\n" ++ numbered
  where
    numberedLines = zip [(1 :: Int)..] . lines $ file

    prependNum (lnum, annLine) =
        let maxDigits = length . show . length $ numberedLines
            lnumStr = show lnum
            paddingNum = maxDigits - length lnumStr
        in replicate paddingNum ' ' ++ lnumStr ++ ": " ++ annLine

    numbered = unlines . map prependNum $ numberedLines

    pi_list = unlines [ show n ++ ": " ++ renderString (displayPatchInfo i)
                      | (n :: Int, i) <- zip [1..] pis ]

    file = concat [ annotation (fst $ head chunk) ++ " | " ++ line (head chunk) ++
                    "\n" ++ unlines [ indent 25 (" | " ++ line l) | l <- tail chunk ]
                  | chunk <- file_ann ]

    pis = nub $ mapMaybe fst $ V.toList a

    pi_map = M.fromList (zip pis [1 :: Int ..])

    file_ann = groupBy ((==) `on` fst) $ zip (V.toList a) (breakLines d)

    line ((_, add), l) = decodeLocale $ BC.concat [l, " ", add]

    annotation (Just i, _) | Just n <- M.lookup i pi_map =
        pad 20 (piMail i) ++ " " ++ pad 4 ('#' : show n)
    annotation _ = pad 25 "unknown"

    pad n str = replicate (n - length str) ' ' ++ take n str

    indent n str = replicate n ' ' ++ str

    piMail pi
        | '<' `elem` piAuthor pi = takeWhile (/= '>') . drop 1 . dropWhile (/= '<') $ piAuthor pi
        | otherwise = piAuthor pi

breakLines :: BC.ByteString -> [BC.ByteString]
breakLines s = case BC.split '\n' s of
    [] -> []
    split | BC.null (last split) -> init split
          | otherwise -> split
