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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Darcs.Test.Patch.Check ( PatchCheck, doCheck, fileExists, dirExists,
                                removeFile, removeDir, createFile, createDir,
                                insertLine, deleteLine, isValid,
                                fileEmpty,
                                checkMove, modifyFile, FileContents(..),
                                inconsistent, handleInconsistent
                              ) where

import Prelude ()
import Darcs.Prelude

import qualified Data.ByteString as B (ByteString)
import Control.Monad.State ( State, evalState )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.State.Class ( get, put, modify, MonadState )
import qualified Data.IntMap as M ( IntMap, mapKeys, delete, insert, empty, lookup, null )

import Darcs.Util.Path
    ( AnchoredPath
    , anchorPath
    , isPrefix
    , movedirfilename
    , parents
    )

-- | File contents are represented by a map from line numbers to line contents.
--   If for a certain line number, the line contents are Nothing, that means
--   that we are sure that that line exists, but we don't know its contents.
--   We must also store the greatest line number that is known to exist in a
--   file, to be able to exclude the possibility of it being empty without
--   knowing its contents.
data FileContents = FC { fcLines   :: M.IntMap B.ByteString
                       , fcMaxline :: Int
                       } deriving (Eq, Show)

data Prop
    = FileEx AnchoredPath
    | DirEx AnchoredPath
    | NotEx AnchoredPath
    | FileLines AnchoredPath FileContents
    deriving (Eq)

instance  Show Prop  where
    show (FileEx f) = "FileEx " ++ anchorPath "" f
    show (DirEx d)  = "DirEx " ++ anchorPath "" d
    show (NotEx f) = "NotEx" ++ anchorPath "" f
    show (FileLines f l)  = "FileLines " ++ anchorPath "" f ++ ": " ++ show l

-- | A simulated repository state. The repository is assumed to be
-- consistent, and it has two lists of properties: one list with properties
-- that hold for this repo, and one with properties that do not hold for this
-- repo. These two lists may not have any common elements: if they had, the
-- repository would be inconsistent.
data ValidState = P [Prop] [Prop] deriving Show

-- | PatchCheck is a 'State' monad with a simulated repository state, wrapped in
-- a 'MaybeT' to indicate failure ('Nothing') or success ('Just').
newtype PatchCheck a = PatchCheck { runPatchCheck :: MaybeT (State ValidState) a }
  deriving (Functor, Applicative, Monad, MonadState ValidState)

-- The existing instance definitions in Control.Monad.Except make it
-- impossible to provide an 'instance MonadError () (MaybeT m)'.

throwPC :: PatchCheck a
throwPC = PatchCheck $ MaybeT $ return Nothing

catchPC :: PatchCheck a -> PatchCheck a -> PatchCheck a
PatchCheck m `catchPC` h =
  PatchCheck $ MaybeT $ do
    a <- runMaybeT m
    case a of
      Nothing -> runMaybeT (runPatchCheck h)
      Just r -> return (Just r)

inconsistent :: PatchCheck ()
inconsistent = throwPC

-- | The @FileContents@ structure for an empty file
emptyFilecontents :: FileContents
emptyFilecontents = FC M.empty 0

-- | Returns a given value if the repository state is inconsistent, and performs
--   a given action otherwise.
handleInconsistent :: a            -- ^ The value to return if the state is inconsistent
                   -> PatchCheck a -- ^ The action to perform otherwise
                   -> PatchCheck a
handleInconsistent v a = a `catchPC` return v

doCheck :: PatchCheck a -> Bool
doCheck p =
  evalState (maybe False (const True) <$> runMaybeT (runPatchCheck p)) (P [] [])

isValid :: PatchCheck ()
isValid = return ()

has :: Prop -> [Prop] -> Bool
has = elem

modifyFile :: AnchoredPath
           -> (Maybe FileContents -> Maybe FileContents)
           -> PatchCheck ()
modifyFile f change = do
    fileExists f
    c <- fileContents f
    case change c of
      Nothing -> assertNot $ FileEx f -- shorthand for "FAIL"
      Just c' -> setContents f c'

insertLine :: AnchoredPath -> Int -> B.ByteString -> PatchCheck ()
insertLine f n l = do
    c <- fileContents f
    case c of
      Nothing -> assertNot $ FileEx f -- in this case, the repo is inconsistent
      Just c' -> do
        let lines'   = M.mapKeys (\k -> if k >= n then k+1 else k) (fcLines c')
            lines''  = M.insert n l lines'
            maxline' = max n (fcMaxline c')
        setContents f (FC lines'' maxline')

-- deletes a line from a hunk patch (third argument) in the given file (first
-- argument) at the given line number (second argument)
deleteLine :: AnchoredPath -> Int -> B.ByteString -> PatchCheck ()
deleteLine f n l = do
    c <- fileContents f
    case c of
      Nothing -> assertNot $ FileEx f
      Just c' ->
        let flines  = fcLines c'
            flines' = M.mapKeys (\k -> if k > n then k-1 else k)
                                (M.delete n flines)
            maxlinenum' | n <= fcMaxline c'  = fcMaxline c' - 1
                        | otherwise           = n - 1
            c'' = FC flines' maxlinenum'
            do_delete = setContents f c''
        in case M.lookup n flines of
          Nothing -> do_delete
          Just l' -> if l == l'
                       then do_delete
                       else assertNot $ FileEx f

setContents :: AnchoredPath -> FileContents -> PatchCheck ()
setContents f c = do
    P ks nots <- get
    let ks' = FileLines f c : filter (not . is_file_lines_for f) ks
    put (P ks' nots)
  where is_file_lines_for file prop = case prop of
                                        FileLines f' _ -> file == f'
                                        _              -> False

-- | Get (as much as we know about) the contents of a file in the current state.
--   Returns Nothing if the state is inconsistent.
fileContents :: AnchoredPath -> PatchCheck (Maybe FileContents)
fileContents f = do
      P ks _ <- get
      return (fic ks)
    where fic (FileLines f' c:_) | f == f' = Just c
          fic (_:ks) = fic ks
          fic [] = Just emptyFilecontents

-- | Checks if a file is empty
fileEmpty :: AnchoredPath          -- ^ Name of the file to check
          -> PatchCheck ()
fileEmpty f = do
  c <- fileContents f
  let empty = case c of
               Just c' -> fcMaxline c' == 0 && M.null (fcLines c')
               Nothing -> True
  if empty
     then setContents f emptyFilecontents
     -- Crude way to make it inconsistent and return false:
     else assertNot $ FileEx f

-- | Replaces a filename by another in all paths. Returns True if the repository
--   is consistent, False if it is not.
doSwap :: AnchoredPath -> AnchoredPath -> PatchCheck ()
doSwap f f' = modify map_sw
  where sw (FileEx a) | f  `isPrefix` a = FileEx $ movedirfilename f f' a
                      | f' `isPrefix` a = FileEx $ movedirfilename f' f a
        sw (DirEx a) | f  `isPrefix` a = DirEx $ movedirfilename f f' a
                     | f' `isPrefix` a = DirEx $ movedirfilename f' f a
        sw (FileLines a c) | f  `isPrefix` a = FileLines (movedirfilename f f' a) c
                           | f' `isPrefix` a = FileLines (movedirfilename f' f a) c
        sw (NotEx a) | f `isPrefix` a = NotEx $ movedirfilename f f' a
                     | f' `isPrefix` a = NotEx $ movedirfilename f' f a
        sw p = p
        map_sw (P ks nots) = P (map sw ks) (map sw nots)

-- | Assert a property about the repository. If the property is already present
-- in the repo state, nothing changes, and the function returns True. If it is
-- not present yet, it is added to the repo state, and the function is True. If
-- the property is already in the list of properties that do not hold for the
-- repo, the state becomes inconsistent, and the function returns false.
assert :: Prop -> PatchCheck ()
assert p = do
    P ks nots <- get
    if has p nots
      then inconsistent
      else if has p ks
             then isValid
             else put (P (p:ks) nots)

-- | Like @assert@, but negatively: state that some property must not hold for
--   the current repo.
assertNot :: Prop -> PatchCheck ()
assertNot p = do
    P ks nots <- get
    if has p ks
      then inconsistent
      else if has p nots
             then isValid
             else put (P ks (p:nots))

-- | Remove a property from the list of properties that do not hold for this
-- repo (if it's there), and add it to the list of properties that hold.
-- Returns False if the repo is inconsistent, True otherwise.
changeToTrue :: Prop -> PatchCheck ()
changeToTrue p = modify filter_nots
  where filter_nots (P ks nots) = P (p:ks) (filter (p /=) nots)

-- | Remove a property from the list of properties that hold for this repo (if
-- it's in there), and add it to the list of properties that do not hold.
-- Returns False if the repo is inconsistent, True otherwise.
changeToFalse :: Prop -> PatchCheck ()
changeToFalse p = do
    modify filter_ks
    where filter_ks (P ks nots) = P (filter (p /=) ks) (p:nots)

assertFileExists :: AnchoredPath -> PatchCheck ()
assertFileExists f =   do assertNot $ NotEx f
                          assertNot $ DirEx f
                          assert $ FileEx f

assertDirExists :: AnchoredPath -> PatchCheck ()
assertDirExists d =   do assertNot $ NotEx d
                         assertNot $ FileEx d
                         assert $ DirEx d

assertExists :: AnchoredPath -> PatchCheck ()
assertExists f = assertNot $ NotEx f

assertNoSuch :: AnchoredPath -> PatchCheck ()
assertNoSuch f =   do assertNot $ FileEx f
                      assertNot $ DirEx f
                      assert $ NotEx f

createFile :: AnchoredPath -> PatchCheck ()
createFile fn = do
  superdirsExist fn
  assertNoSuch fn
  changeToTrue (FileEx fn)
  changeToFalse (NotEx fn)

createDir :: AnchoredPath -> PatchCheck ()
createDir fn = do
  substuffDontExist fn
  superdirsExist fn
  assertNoSuch fn
  changeToTrue (DirEx fn)
  changeToFalse (NotEx fn)

removeFile :: AnchoredPath -> PatchCheck ()
removeFile fn = do
  superdirsExist fn
  assertFileExists fn
  fileEmpty fn
  changeToFalse (FileEx fn)
  changeToTrue (NotEx fn)

removeDir :: AnchoredPath -> PatchCheck ()
removeDir fn = do
  substuffDontExist fn
  superdirsExist fn
  assertDirExists fn
  changeToFalse (DirEx fn)
  changeToTrue (NotEx fn)

checkMove :: AnchoredPath -> AnchoredPath -> PatchCheck ()
checkMove f f' = do
  superdirsExist f
  superdirsExist f'
  assertExists f
  assertNoSuch f'
  doSwap f f'

substuffDontExist :: AnchoredPath -> PatchCheck ()
substuffDontExist d = do
    P ks _ <- get
    if all noss ks
      then isValid
      else inconsistent
  where noss (FileEx f) = not (is_within_dir f)
        noss (DirEx f) = not (is_within_dir f)
        noss _ = True
        is_within_dir f = d `isPrefix` f && d /= f

superdirsExist :: AnchoredPath -> PatchCheck ()
superdirsExist fn = mapM_ assertDirExists (parents fn)

fileExists :: AnchoredPath -> PatchCheck ()
fileExists fn = do
  superdirsExist fn
  assertFileExists fn

dirExists :: AnchoredPath -> PatchCheck ()
dirExists fn = do
  superdirsExist fn
  assertDirExists fn
