--  Copyright (C) 2002-2005 David Roundy
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

-- |
-- Module      : Darcs.Patch.Apply
-- Copyright   : 2002-2005 David Roundy
-- License     : GPL
-- Maintainer  : darcs-devel@darcs.net
-- Stability   : experimental
-- Portability : portable

module Darcs.Patch.Apply
    (
      Apply(..)
    , applyToPaths
    , applyToTree
    , applyToState
    , maybeApplyToTree
    , effectOnPaths
    ) where

import Darcs.Prelude

import Control.Exception ( catch, IOException )

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Util.Tree ( Tree )

import Darcs.Patch.ApplyMonad ( ApplyMonad(..), withFileNames, ApplyMonadTrans(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..) )

class Apply p where
    type ApplyState p :: (* -> *) -> *
    apply :: ApplyMonad (ApplyState p) m => p wX wY -> m ()
    unapply :: ApplyMonad (ApplyState p) m => p wX wY -> m ()
    default unapply :: (ApplyMonad (ApplyState p) m, Invert p) => p wX wY -> m ()
    unapply = apply . invert

instance Apply p => Apply (FL p) where
    type ApplyState (FL p) = ApplyState p
    apply NilFL = return ()
    apply (p:>:ps) = apply p >> apply ps
    unapply NilFL = return ()
    unapply (p:>:ps) = unapply ps >> unapply p

instance Apply p => Apply (RL p) where
    type ApplyState (RL p) = ApplyState p
    apply NilRL = return ()
    apply (ps:<:p) = apply ps >> apply p
    unapply NilRL = return ()
    unapply (ps:<:p) = unapply p >> unapply ps


effectOnPaths :: (Apply p, ApplyState p ~ Tree)
              => p wX wY
              -> [AnchoredPath]
              -> [AnchoredPath]
effectOnPaths p fps = fps' where
    (_, fps', _) = applyToPaths p Nothing fps

applyToPaths :: (Apply p, ApplyState p ~ Tree)
             => p wX wY
             -> Maybe [(AnchoredPath, AnchoredPath)]
             -> [AnchoredPath]
             -> ([AnchoredPath], [AnchoredPath], [(AnchoredPath, AnchoredPath)])
applyToPaths pa ofpos fs = withFileNames ofpos fs (apply pa)

-- | Apply a patch to a 'Tree', yielding a new 'Tree'.
applyToTree :: (Apply p, Monad m, ApplyState p ~ Tree)
            => p wX wY
            -> Tree m
            -> m (Tree m)
applyToTree = applyToState

applyToState :: forall p m wX wY. (Apply p, ApplyMonadTrans (ApplyState p) m)
             => p wX wY
             -> (ApplyState p) m
             -> m ((ApplyState p) m)
applyToState patch t = snd <$> runApplyMonad (apply patch) t

-- | Attempts to apply a given patch to a Tree. If the apply fails, we return
-- Nothing, otherwise we return the updated Tree.
maybeApplyToTree :: (Apply p, ApplyState p ~ Tree) => p wX wY -> Tree IO
                 -> IO (Maybe (Tree IO))
maybeApplyToTree patch tree =
    (Just `fmap` applyToTree patch tree) `catch` (\(_ :: IOException) -> return Nothing)
