--  Copyright (C) 2002-2003,2007 David Roundy
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

module Darcs.Patch.Prim.V1.Core
       ( Prim(..),
         DirPatchType(..), FilePatchType(..),
         isIdentity,
         comparePrim,
       )
       where

import Darcs.Prelude

import qualified Data.ByteString as B (ByteString)

import Darcs.Util.Path ( AnchoredPath )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Debug ( PatchDebug(..) )
import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( PrimConstruct(..), PrimClassify(..) )

data Prim wX wY where
    Move :: !AnchoredPath -> !AnchoredPath -> Prim wX wY
    DP :: !AnchoredPath -> !(DirPatchType wX wY) -> Prim wX wY
    FP :: !AnchoredPath -> !(FilePatchType wX wY) -> Prim wX wY
    ChangePref :: !String -> !String -> !String -> Prim wX wY

data FilePatchType wX wY
    = RmFile
    | AddFile
    | Hunk !Int [B.ByteString] [B.ByteString]
    | TokReplace !String !String !String
    | Binary B.ByteString B.ByteString
    deriving (Eq,Ord)

type role FilePatchType nominal nominal

data DirPatchType wX wY = RmDir | AddDir
                           deriving (Eq,Ord)

type role DirPatchType nominal nominal

instance Eq2 FilePatchType where
    unsafeCompare a b = a == unsafeCoerceP b

instance Invert FilePatchType where
    invert RmFile = AddFile
    invert AddFile = RmFile
    invert (Hunk line old new) = Hunk line new old
    invert (TokReplace t o n) = TokReplace t n o
    invert (Binary o n) = Binary n o

instance Eq2 DirPatchType where
    unsafeCompare a b = a == unsafeCoerceP b

instance Invert DirPatchType where
    invert RmDir = AddDir
    invert AddDir = RmDir

isIdentity :: Prim wX wY -> EqCheck wX wY
isIdentity (FP _ (Binary old new)) | old == new = unsafeCoerceP IsEq
isIdentity (FP _ (Hunk _ old new)) | old == new = unsafeCoerceP IsEq
isIdentity (FP _ (TokReplace _ old new)) | old == new = unsafeCoerceP IsEq
isIdentity (Move old new) | old == new = unsafeCoerceP IsEq
isIdentity _ = NotEq

instance PrimClassify Prim where
   primIsAddfile (FP _ AddFile) = True
   primIsAddfile _ = False

   primIsRmfile (FP _ RmFile) = True
   primIsRmfile _ = False

   primIsAdddir (DP _ AddDir) = True
   primIsAdddir _ = False

   primIsRmdir (DP _ RmDir) = True
   primIsRmdir _ = False

   primIsMove (Move _ _) = True
   primIsMove _ = False

   primIsHunk (FP _ (Hunk _ _ _)) = True
   primIsHunk _ = False

   primIsTokReplace (FP _ (TokReplace _ _ _)) = True
   primIsTokReplace _ = False

   primIsBinary (FP _ (Binary _ _)) = True
   primIsBinary _ = False

   primIsSetpref (ChangePref _ _ _) = True
   primIsSetpref _ = False

   is_filepatch (FP f _) = Just f
   is_filepatch _ = Nothing

evalargs :: (a -> b -> c) -> a -> b -> c
evalargs f x y = (f $! x) $! y

instance PrimConstruct Prim where
   addfile f = FP f AddFile
   rmfile f = FP f RmFile
   adddir d = DP d AddDir
   rmdir d = DP d RmDir
   move old new = Move old new
   changepref p f t = ChangePref p f t
   hunk f line old new = evalargs FP f (Hunk line old new)
   tokreplace f tokchars old new =
       evalargs FP f (TokReplace tokchars old new)
   binary f old new = FP f $ Binary old new
   primFromHunk (FileHunk f line before after) = FP f (Hunk line before after)

instance IsHunk Prim where
   isHunk (FP f (Hunk line before after)) = Just (FileHunk f line before after)
   isHunk _ = Nothing

instance Invert Prim where
    invert (FP f p)  = FP f (invert p)
    invert (DP d p) = DP d (invert p)
    invert (Move f f') = Move f' f
    invert (ChangePref p f t) = ChangePref p t f

instance PatchInspect Prim where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (Move f1 f2) = [f1, f2]
    listTouchedFiles (FP f _) = [f]
    listTouchedFiles (DP d _) = [d]
    listTouchedFiles (ChangePref _ _ _) = []

    hunkMatches f (FP _ (Hunk _ remove add)) = anyMatches remove || anyMatches add
        where anyMatches = foldr ((||) . f) False
    hunkMatches _ (FP _ _) = False
    hunkMatches _ (DP _ _) = False
    hunkMatches _ (ChangePref _ _ _) = False
    hunkMatches _ (Move _ _) = False

instance PatchDebug Prim

instance Eq2 Prim where
    unsafeCompare (Move a b) (Move c d) = a == c && b == d
    unsafeCompare (DP d1 p1) (DP d2 p2)
        = d1 == d2 && p1 `unsafeCompare` p2
    unsafeCompare (FP f1 fp1) (FP f2 fp2)
        = f1 == f2 && fp1 `unsafeCompare` fp2
    unsafeCompare (ChangePref a1 b1 c1) (ChangePref a2 b2 c2)
        = c1 == c2 && b1 == b2 && a1 == a2
    unsafeCompare _ _ = False

instance Eq (Prim wX wY) where
    (==) = unsafeCompare

-- | 'comparePrim' @p1 p2@ is used to provide an arbitrary ordering between
--   @p1@ and @p2@.  Basically, identical patches are equal and
--   @Move < DP < FP < ChangePref@.
--   Everything else is compared in dictionary order of its arguments.
comparePrim :: Prim wX wY -> Prim wW wZ -> Ordering
comparePrim (Move a b) (Move c d) = compare (a, b) (c, d)
comparePrim (Move _ _) _ = LT
comparePrim _ (Move _ _) = GT
comparePrim (DP d1 p1) (DP d2 p2) = compare (d1, p1) $ unsafeCoerceP (d2, p2)
comparePrim (DP _ _) _ = LT
comparePrim _ (DP _ _) = GT
comparePrim (FP f1 fp1) (FP f2 fp2) = compare (f1, fp1) $ unsafeCoerceP (f2, fp2)
comparePrim (FP _ _) _ = LT
comparePrim _ (FP _ _) = GT
comparePrim (ChangePref a1 b1 c1) (ChangePref a2 b2 c2)
 = compare (c1, b1, a1) (c2, b2, a2)
