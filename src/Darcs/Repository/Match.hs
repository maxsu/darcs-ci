--  Copyright (C) 2004-2005 David Roundy
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

module Darcs.Repository.Match
    (
      getRecordedUpToMatch
    , getOnePatchset
    ) where

import Darcs.Prelude

import Darcs.Patch.Match
    ( rollbackToPatchSetMatch
    , PatchSetMatch(..)
    , getMatchingTag
    , matchAPatchset
    )

import Darcs.Patch.Bundle ( readContextFile )
import Darcs.Patch.ApplyMonad ( ApplyMonad(..) )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch ( RepoPatch, IsRepoType )
import Darcs.Patch.Set ( Origin, PatchSet(..), SealedPatchSet, patchSetDrop )

import Darcs.Repository.Flags
    ( WithWorkingDir (WithWorkingDir) )
import Darcs.Repository.ApplyPatches ( DefaultIO, runDefault )
import Darcs.Repository.InternalTypes ( Repository )
import Darcs.Repository.Hashed ( readRepo )
import Darcs.Repository.Pristine ( createPristineDirectoryTree )

import Darcs.Util.Tree ( Tree )

import Darcs.Util.Path ( toFilePath )

-- | Create a new pristine and working tree in the current working directory,
-- corresponding to the state of the 'PatchSet' returned by 'getOnePatchSet'
-- for the same 'PatchSetMatch'.
getRecordedUpToMatch :: (ApplyMonad (ApplyState p) DefaultIO, IsRepoType rt, RepoPatch p, ApplyState p ~ Tree)
                     => Repository rt p wR wU wT
                     -> PatchSetMatch
                     -> IO ()
getRecordedUpToMatch r = withRecordedMatch r . rollbackToPatchSetMatch

getOnePatchset :: (IsRepoType rt, RepoPatch p)
               => Repository rt p wR wU wR
               -> PatchSetMatch
               -> IO (SealedPatchSet rt p Origin)
getOnePatchset repository pm =
  case pm of
    IndexMatch n -> patchSetDrop (n-1) <$> readRepo repository
    PatchMatch m -> matchAPatchset m <$> readRepo repository
    TagMatch m -> getMatchingTag m <$> readRepo repository
    ContextMatch path -> do
      ref <- readRepo repository
      readContextFile ref (toFilePath path)

withRecordedMatch :: (IsRepoType rt, RepoPatch p)
                  => Repository rt p wR wU wT
                  -> (PatchSet rt p Origin wR -> DefaultIO ())
                  -> IO ()
withRecordedMatch r job
    = do createPristineDirectoryTree r "." WithWorkingDir
         readRepo r >>= runDefault . job
