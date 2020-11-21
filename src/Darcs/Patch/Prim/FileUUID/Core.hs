-- Copyright (C) 2011 Petr Rockai
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


module Darcs.Patch.Prim.FileUUID.Core
    ( Prim(..)
    , Hunk(..)
    , HunkMove(..)
    -- re-exports
    , Object(..)
    , UUID(..)
    , Location(..)
    , Name
    , FileContent
    ) where

import Darcs.Prelude

import Darcs.Patch.Witnesses.Eq ( Eq2(..) )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Unsafe
import Darcs.Patch.FileHunk( IsHunk(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Prim.Class ( PrimConstruct(..), PrimClassify(..) )
import Darcs.Patch.Prim.FileUUID.ObjectMap

-- -----------------------------------------------------------------------------
-- Hunk

data Hunk wX wY = H !Int !FileContent !FileContent
  deriving (Eq, Show)

type role Hunk nominal nominal

instance Show1 (Hunk wX)

instance Show2 Hunk

invertHunk :: Hunk wX wY -> Hunk wY wX
invertHunk (H off old new) = H off new old

instance Eq2 Hunk where
  unsafeCompare p q = unsafeCoerceP p == q

-- -----------------------------------------------------------------------------
-- HunkMove

data HunkMove wX wY = HM !UUID !Int !UUID !Int !FileContent
  deriving (Eq, Show)

type role HunkMove nominal nominal

invertHunkMove :: HunkMove wX wY -> HunkMove wY wX
invertHunkMove (HM sid soff tid toff content) = HM tid toff sid soff content

instance Eq2 HunkMove where
  unsafeCompare (HM sid1 soff1 tid1 toff1 c1) (HM sid2 soff2 tid2 toff2 c2) =
    sid1 == sid2 && soff1 == soff2 && tid1 == tid2 && toff1 == toff2 && c1 == c2

-- -----------------------------------------------------------------------------
-- Prim

data Prim wX wY where
  Hunk :: !UUID -> !(Hunk wX wY) -> Prim wX wY
  HunkMove :: !(HunkMove wX wY) -> Prim wX wY
  Manifest :: !UUID -> !Location -> Prim wX wY
  Demanifest :: !UUID -> !Location -> Prim wX wY
  Identity :: Prim wX wX

deriving instance Eq (Prim wX wY)
deriving instance Show (Prim wX wY)

instance Show1 (Prim wX)

instance Show2 Prim

-- TODO: PrimClassify doesn't make sense for FileUUID prims
instance PrimClassify Prim where
  primIsAddfile _ = False
  primIsRmfile _ = False
  primIsAdddir _ = False
  primIsRmdir _ = False
  primIsHunk _ = False
  primIsMove _ = False
  primIsBinary _ = False
  primIsTokReplace _ = False
  primIsSetpref _ = False
  is_filepatch _ = Nothing

-- TODO: PrimConstruct makes no sense for FileUUID prims
instance PrimConstruct Prim where
  addfile _ = error "PrimConstruct addfile"
  rmfile _ = error "PrimConstruct rmfile"
  adddir _ = error "PrimConstruct adddir"
  rmdir _ = error "PrimConstruct rmdir"
  move _ _ = error "PrimConstruct move"
  changepref _ _ _ = error "PrimConstruct changepref"
  hunk _ _ _ _ = error "PrimConstruct hunk"
  tokreplace _ _ _ _ = error "PrimConstruct tokreplace"
  binary _ _ _ = error "PrimConstruct binary"
  primFromHunk _ = error "PrimConstruct primFromHunk"

instance IsHunk Prim where
  isHunk _ = Nothing

instance Invert Prim where
  invert (Hunk x h) = Hunk x $ invertHunk h
  invert (HunkMove hm) = HunkMove $ invertHunkMove hm
  invert (Manifest x y) = Demanifest x y
  invert (Demanifest x y) = Manifest x y
  invert Identity = Identity

instance PatchInspect Prim where
  -- We don't need this for FileUUID. Slashes are not allowed in Manifest and
  -- Demanifest patches and nothing else uses working-copy paths.
  listTouchedFiles _ = []

  -- TODO (used for --match 'hunk ...', presumably)
  hunkMatches _ _ = False

instance Eq2 Prim where
  unsafeCompare (Hunk a b) (Hunk c d) = a == c && b `unsafeCompare` d
  unsafeCompare (Manifest a b) (Manifest c d) = a == c && b == d
  unsafeCompare (Demanifest a b) (Demanifest c d) = a == c && b == d
  unsafeCompare Identity Identity = True
  unsafeCompare _ _ = False
