--  Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Commands.Util.Tree
    ( 
    -- * Tree lookup.
      treeHas
    , treeHasDir
    , treeHasFile
    , treeHasAnycase
    ) where

import Darcs.Prelude

import Control.Monad ( forM )
import Control.Monad.State.Strict( gets )
import Data.Maybe ( fromMaybe )

import qualified Darcs.Util.Tree.Monad as TM
    ( TreeMonad, withDirectory, fileExists, directoryExists
    , virtualTreeMonad, currentDirectory, exists, tree )
import Darcs.Util.Tree ( Tree, listImmediate, findTree )

import Darcs.Util.Path
    ( AnchoredPath(..), eqAnycase )

treeHasAnycase :: Monad m
               => Tree m
               -> AnchoredPath
               -> m Bool
treeHasAnycase tree path =
    fst `fmap` TM.virtualTreeMonad (existsAnycase path) tree


existsAnycase :: Monad m
              => AnchoredPath
              -> TM.TreeMonad m Bool
existsAnycase (AnchoredPath []) = return True
existsAnycase (AnchoredPath (x:xs)) = do
     wd <- TM.currentDirectory
     tree <- fromMaybe (error "invalid path passed to existsAnycase") <$>
             gets (flip findTree wd . TM.tree)
     let subs = [ AnchoredPath [n] | (n, _) <- listImmediate tree,
                                          eqAnycase n x ]
     or `fmap` forM subs (\path -> do
       file <- TM.fileExists path
       if file then return True
               else TM.withDirectory path (existsAnycase $ AnchoredPath xs))


treeHas :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHas tree path = fst `fmap` TM.virtualTreeMonad (TM.exists path) tree

treeHasDir :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHasDir tree path = fst `fmap` TM.virtualTreeMonad (TM.directoryExists path) tree

treeHasFile :: Monad m => Tree m -> AnchoredPath -> m Bool
treeHasFile tree path = fst `fmap` TM.virtualTreeMonad (TM.fileExists path) tree
