-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
module Darcs.Repository.InternalTypes ( Repository, PristineType(..)
                                      , repoCache, modifyCache
                                      , repoFormat
                                      , repoLocation
                                      , withRepoLocation
                                      , repoPristineType
                                      , unsafeCoerceRepoType
                                      , unsafeCoercePatchType
                                      , unsafeCoerceR
                                      , unsafeCoerceU
                                      , unsafeCoerceT
                                      , mkRepo
                                      ) where

import Darcs.Prelude

import Darcs.Repository.Cache ( Cache )
import Darcs.Repository.Format ( RepoFormat )
import Darcs.Patch ( RepoType )
import Darcs.Util.File ( withCurrentDirectory )
import Unsafe.Coerce ( unsafeCoerce )

data PristineType
  = NoPristine
  | PlainPristine
  | HashedPristine
    deriving ( Show, Eq )

-- |A @Repository@ is a token representing the state of a repository on disk.
-- It is parameterized by the patch type in the repository, and witnesses for
-- the recorded state of the repository (i.e. what darcs get would retrieve),
-- the unrecorded state (what's in the working tree now),
-- and the tentative state, which represents work in progress that will
-- eventually become the new recorded state unless something goes wrong.
data Repository (rt :: RepoType) (p :: * -> * -> *) wRecordedstate wUnrecordedstate wTentativestate =
  Repo !String !RepoFormat !PristineType Cache deriving ( Show )

type role Repository nominal nominal nominal nominal nominal

repoLocation :: Repository rt p wR wU wT -> String
repoLocation (Repo loc _ _ _) = loc

withRepoLocation :: Repository rt p wR wU wT -> IO a -> IO a
withRepoLocation repo = withCurrentDirectory (repoLocation repo)

repoFormat :: Repository rt p wR wU wT -> RepoFormat
repoFormat (Repo _ fmt _ _) = fmt

repoPristineType :: Repository rt p wR wU wT -> PristineType
repoPristineType (Repo _ _ pr _) = pr

repoCache :: Repository rt p wR wU wT -> Cache
repoCache (Repo _ _ _ c) = c

modifyCache :: (Cache -> Cache) -> Repository rt p wR wU wT -> Repository rt p wR wU wT
modifyCache g (Repo l f p c) = Repo l f p (g c)

unsafeCoerceRepoType :: Repository rt p wR wU wT -> Repository rt' p wR wU wT
unsafeCoerceRepoType = unsafeCoerce

unsafeCoercePatchType :: Repository rt p wR wU wT -> Repository rt p' wR wU wT
unsafeCoercePatchType = unsafeCoerce

unsafeCoerceR :: Repository rt p wR wU wT -> Repository rt p wR' wU wT
unsafeCoerceR = unsafeCoerce

unsafeCoerceU :: Repository rt p wR wU wT -> Repository rt p wR wU' wT
unsafeCoerceU = unsafeCoerce

unsafeCoerceT :: Repository rt p wR wU wT -> Repository rt p wR wU wT'
unsafeCoerceT = unsafeCoerce

mkRepo :: String -> RepoFormat -> PristineType -> Cache -> Repository rt p wR wU wT
mkRepo = Repo
