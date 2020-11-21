-- Copyright (C) 2009 Benedikt Schmidt
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Darcs.Patch.Index.Monad
    ( withPatchMods
    , applyToFileMods
    , makePatchID
    ) where

import Darcs.Prelude

import Darcs.Patch.Index.Types ( PatchMod(..), PatchId(..) )
import Darcs.Patch.Info ( makePatchname, PatchInfo )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..), ApplyMonadTree(..) )
import Control.Monad.State
import Control.Arrow
import Darcs.Util.Path ( AnchoredPath, anchorPath, movedirfilename, isPrefix )
import qualified Data.Set as S
import Data.Set ( Set )
import Darcs.Util.Tree (Tree)

newtype FileModMonad a = FMM (State (Set AnchoredPath, [PatchMod AnchoredPath]) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (Set AnchoredPath, [PatchMod AnchoredPath])
           )

withPatchMods :: FileModMonad a
              -> Set AnchoredPath
              -> (Set AnchoredPath, [PatchMod AnchoredPath])
withPatchMods (FMM m) fps = second reverse $ execState m (fps,[])

-- These instances are defined to be used only with
-- apply.
instance ApplyMonad Tree FileModMonad where
    type ApplyMonadBase FileModMonad = FileModMonad
    nestedApply _ _ = error "nestedApply FileModMonad"
    liftApply _ _ = error "liftApply FileModMonad"
    getApplyState = error "getApplyState FileModMonad"

instance ApplyMonadTree FileModMonad where
    mDoesDirectoryExist d = do
      fps <- gets fst
      return $ S.member d fps
    mDoesFileExist f = do
      fps <- gets fst
      return $ S.member f fps
    mReadFilePS _ = error "mReadFilePS FileModMonad"
    mCreateFile = createFile
    mCreateDirectory = createDir
    mRemoveFile = remove
    mRemoveDirectory = remove
    mRename a b = do
      fns <- gets fst
      if S.notMember a fns then
         addMod (PInvalid a)  -- works around some old repo inconsistencies
       else
        do -- we have to account for directory moves
           addMod (PRename a b)
           modifyFps (S.delete a)
           addFile b
           forM_ (S.toList fns) $ \fn ->
             when (a `isPrefix` fn && a /= fn) $ do
               modifyFps (S.delete fn)
               let newfn = movedirfilename a b fn
               addFile newfn
               addMod (PRename fn newfn)

    mModifyFilePS f _ = addMod (PTouch f)

-- ---------------------------------------------------------------------
-- State Handling Functions

addMod :: PatchMod AnchoredPath -> FileModMonad ()
addMod pm = modify $ second (pm :)

addFile :: AnchoredPath -> FileModMonad ()
addFile f = modifyFps (S.insert f)

createFile :: AnchoredPath -> FileModMonad ()
createFile fn = do
  errorIfPresent fn True
  addMod (PCreateFile fn)
  addFile fn

createDir :: AnchoredPath -> FileModMonad ()
createDir fn = do
  errorIfPresent fn False
  addMod (PCreateDir fn)
  addFile fn

errorIfPresent :: AnchoredPath -> Bool -> FileModMonad ()
errorIfPresent fn isFile = do
    fs <- gets fst
    when (S.member fn fs) $
        error $ unwords [ "error: patch index entry for"
                        , if isFile then "file" else "directory"
                        , anchorPath "" fn
                        , "created >1 times. Run `darcs repair` and try again."
                        ]

remove :: AnchoredPath -> FileModMonad ()
remove f = addMod (PRemove f) >> modifyFps (S.delete f)

modifyFps :: (Set AnchoredPath -> Set AnchoredPath) -> FileModMonad ()
modifyFps f = modify $ first f

makePatchID :: PatchInfo -> PatchId
makePatchID = PID . makePatchname

--------------------------------------------------------------------------------
-- | Apply a patch to set of 'AnchoredPath's, yielding the new set of
-- 'AnchoredPath's and 'PatchMod's
applyToFileMods :: (Apply p, ApplyState p ~ Tree)
                => p wX wY
                -> Set AnchoredPath
                -> (Set AnchoredPath, [PatchMod AnchoredPath])
applyToFileMods patch = withPatchMods (apply patch)
