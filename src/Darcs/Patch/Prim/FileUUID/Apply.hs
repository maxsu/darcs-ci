{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module Darcs.Patch.Prim.FileUUID.Apply ( hunkEdit, ObjectMap(..) ) where

import Darcs.Prelude

import Control.Monad.State( StateT, runStateT, gets, lift, put )
import qualified Data.ByteString as B
import qualified Data.Map as M

import Debug.Trace ( trace )
-- import Text.Show.Pretty ( ppShow )

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.ApplyMonad
    ( ApplyMonad(..), ApplyMonadTrans(..)
    , ToTree(..), ApplyMonadState(..)
    )
import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.FileUUID.Core ( Prim(..), Hunk(..), HunkMove(..) )
import Darcs.Patch.Prim.FileUUID.Show
import Darcs.Patch.Prim.FileUUID.ObjectMap
import Darcs.Patch.Repair ( RepairToFL(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..) )

import Darcs.Util.Hash( Hash(..) )
import Darcs.Util.Printer( text, packedString, ($$), renderString )


instance Apply Prim where
  type ApplyState Prim = ObjectMap
  apply (Manifest i (L dirid name)) = editDirectory dirid (M.insert name i)
  apply (Demanifest _ (L dirid name)) = editDirectory dirid (M.delete name)
  apply (Hunk i hunk) = editFile i (hunkEdit hunk)
  apply (HunkMove (HM fs ls ft lt c)) =
    editFile fs (hunkEdit (H ls c B.empty)) >> editFile ft (hunkEdit (H lt B.empty c))
  apply Identity = return ()

instance RepairToFL Prim where
  applyAndTryToFixFL p = apply p >> return Nothing

instance PrimApply Prim where
  applyPrimFL NilFL = return ()
  applyPrimFL (p :>: ps) = apply p >> applyPrimFL ps

instance ToTree ObjectMap -- TODO

hunkEdit :: Hunk wX wY -> FileContent -> FileContent
hunkEdit h@(H off old new) c
  | old `B.isPrefixOf` (B.drop off c) =
      B.concat [B.take off c, new, B.drop (off + B.length old) c]
  | otherwise = error $ renderString $
      text "##error applying hunk:" $$ displayHunk Nothing h $$ "##to" $$
      packedString c
--       $$ text "##old=" <> text (ppShow old) $$
--       text "##new=" <> text (ppShow new) $$
--       text "##c=" <> text (ppShow c)

editObject :: Monad m
           => UUID
           -> (Maybe (Object m) -> Object m)
           -> (StateT (ObjectMap m) m) ()
editObject i edit = do
  load <- gets getObject
  store <- gets putObject
  obj <- lift $ load i
  new <- lift $ store i $ edit obj
  put new

-- a semantic, ObjectMap-based interface for patch application
class ApplyMonadObjectMap m where
  editFile :: UUID -> (FileContent -> FileContent) -> m ()
  editDirectory :: UUID -> (DirContent -> DirContent) -> m ()

instance ApplyMonadState ObjectMap where
  type ApplyMonadStateOperations ObjectMap = ApplyMonadObjectMap

instance (Monad m) => ApplyMonad ObjectMap (StateT (ObjectMap m) m) where
  type ApplyMonadBase (StateT (ObjectMap m) m) = m

instance (Monad m) => ApplyMonadObjectMap (StateT (ObjectMap m) m) where
  editFile i edit = editObject i edit'
    where
      edit' (Just (Blob x _)) = Blob (edit `fmap` x) NoHash
      edit' Nothing = Blob (return $ edit "") NoHash
      edit' (Just d@(Directory m)) =
        trace ("\neditFile called with Directory object: " ++ show (i,m) ++ "\n") d
  editDirectory i edit = editObject i edit'
    where
      edit' (Just (Directory x)) = Directory $ edit x
      edit' Nothing = Directory $ edit M.empty
      edit' (Just b@(Blob _ h)) =
        trace ("\neditDirectory called with File object: " ++ show (i,h) ++ "\n") b

instance (Monad m) => ApplyMonadTrans ObjectMap m where
  type ApplyMonadOver ObjectMap m = StateT (ObjectMap m) m
  runApplyMonad = runStateT
