--  Copyright (C) 2011-2 Ganesh Sittampalam
--
--  BSD3

module Darcs.Patch.Rebase.Name
    ( RebaseName(..)
    , commuteNamePrim, commutePrimName
    , commuterIdNamed, commuterNamedId
    , commuteNameNamed, commuteNamedName
    , pushFixupName
    ) where

import Darcs.Prelude

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.CommuteFn ( CommuteFn, commuterIdFL, commuterFLId )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo, readPatchInfo )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..) )
import Darcs.Patch.Witnesses.Ordered ( (:>)(..), FL(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..) )
import Darcs.Patch.Witnesses.Show ( Show1, Show2 )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )

import Darcs.Patch.Rebase.PushFixup ( PushFixupFn )

import Darcs.Util.Parser ( lexString )
import Darcs.Util.Printer ( empty, blueText, ($$) )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString.Char8 as BC ( pack )

-- Note: in principle this is a general concept not limited to
-- rebase, and we might be able to generalise this type and
-- refactor named patches to use it too.

-- | A 'RebaseName' encapsulates the concept of the name of a patch,
-- without any contents. This allows us to track explicit dependencies
-- in the rebase state, changing them to follow uses of amend-record
-- or unsuspend on a depended-on patch, and warning the user if any
-- are lost entirely.
data RebaseName wX wY where
  AddName :: PatchInfo -> RebaseName wX wY
  DelName :: PatchInfo -> RebaseName wX wY
  Rename :: PatchInfo -> PatchInfo -> RebaseName wX wY
    deriving (Eq, Show)

instance Show1 (RebaseName wX)

instance Show2 RebaseName

instance ShowPatchBasic RebaseName where
   showPatch f (AddName n) = blueText "addname" $$ showPatchInfo f n
   showPatch f (DelName n) = blueText "delname" $$ showPatchInfo f n
   showPatch f (Rename old new) = blueText "rename" $$ showPatchInfo f old $$ showPatchInfo f new

instance ShowPatch RebaseName where
   summary _ = empty -- TODO improve this?
   summaryFL _ = empty

instance ReadPatch RebaseName where
   readPatch' = readAddName <|> readDelName <|> readRename
     where
       readAddName = do lexString (BC.pack "addname")
                        n <- readPatchInfo
                        return (Sealed (AddName n))
       readDelName = do lexString (BC.pack "delname")
                        n <- readPatchInfo
                        return (Sealed (DelName n))
       readRename  = do lexString (BC.pack "rename")
                        old <- readPatchInfo
                        new <- readPatchInfo
                        return (Sealed (Rename old new))

instance Commute RebaseName where
   commute (AddName n1 :> AddName n2)
      | n1 == n2 = error "impossible case"
      | otherwise = Just (AddName n2 :> AddName n1)
   commute (DelName n1 :> DelName n2)
      | n1 == n2 = error "impossible case"
      | otherwise = Just (DelName n2 :> DelName n1)
   commute (AddName n1 :> DelName n2)
      | n1 /= n2 = Just (DelName n2 :> AddName n1)
      | otherwise = Nothing
   commute (DelName n1 :> AddName n2)
      | n1 /= n2 = Just (AddName n2 :> DelName n1)
      | otherwise = Nothing
   commute (Rename old new :> AddName n)
      | n == old = Nothing
      | n == new = error "impossible case" -- precondition of Add is that n doesn't exist
      | otherwise = Just (AddName n :> Rename old new)
   commute (AddName n :> Rename old new)
      | n == old = Nothing
      | n == new = error "impossible case" -- precondition of Rename is that new doesn't exist
      | otherwise = Just (Rename old new :> AddName n)
   commute (Rename old new :> DelName n)
      | n == old = error "impossible case" -- precondition of Del is that n does exist
      | n == new = Nothing
      | otherwise = Just (DelName n :> Rename old new)
   commute (DelName n :> Rename old new)
      | n == old = error "impossible case" -- precondition of Rename is that old does exist
      | n == new = Nothing
      | otherwise = Just (Rename old new :> DelName n)
   commute (Rename old1 new1 :> Rename old2 new2)
      | old1 == old2 = error "impossible case"
      | new1 == new2 = error "impossible case"
      | old1 == new2 = Nothing
      | new1 == old2 = Nothing
      | otherwise = Just (Rename old2 new2 :> Rename old1 new1)

instance Invert RebaseName where
   invert (AddName n) = DelName n
   invert (DelName n) = AddName n
   invert (Rename old new) = Rename new old

instance PatchInspect RebaseName where
    listTouchedFiles _ = []
    hunkMatches _ _ = False

instance Eq2 RebaseName where
   p1 =\/= p2
      | p1 == unsafeCoercePEnd p2 = unsafeCoercePEnd IsEq
      | otherwise = NotEq

-- |Commute a 'RebaseName' and a primitive patch. They trivially
-- commute so this just involves changing the witnesses.
-- This is unsafe if the patch being commuted actually has a
-- name (e.g. Named or PatchInfo - PrimWithName is ok),
commuteNamePrim :: (RebaseName :> prim) wX wY -> (prim :> RebaseName) wX wY
commuteNamePrim (n :> f) = unsafeCoerceP f :> unsafeCoerceP n

-- |Commute a primitive patch and a 'RebaseName'. They trivially
-- commute so this just involves changing the witnesses.
-- This is unsafe if the patch being commuted actually has a
-- name (e.g. Named or PatchInfo - PrimWithName is ok),
commutePrimName :: (prim :> RebaseName) wX wY -> (RebaseName :> prim) wX wY
commutePrimName (f :> n) = unsafeCoerceP n :> unsafeCoerceP f

-- commuterIdNamed and commuterNamedId are defined here rather than in
-- Named given that they are unsafe, to reduce the chances of them
-- being used inappropriately.

-- |Commute an unnamed patch with a named patch. This is unsafe if the
-- second patch actually does have a name (e.g. Named, PatchInfoAnd, etc),
-- as it won't check the explicit dependencies.
commuterIdNamed :: CommuteFn p1 p2 -> CommuteFn p1 (Named p2)
commuterIdNamed commuter (p1 :> NamedP n2 d2 p2) =
   do p2' :> p1' <- commuterIdFL commuter (p1 :> p2)
      return (NamedP n2 d2 p2' :> p1')

-- |Commute an unnamed patch with a named patch. This is unsafe if the
-- first patch actually does have a name (e.g. Named, PatchInfoAnd, etc),
-- as it won't check the explicit dependencies.
commuterNamedId :: CommuteFn p1 p2 -> CommuteFn (Named p1) p2
commuterNamedId commuter (NamedP n1 d1 p1 :> p2) =
   do p2' :> p1' <- commuterFLId commuter (p1 :> p2)
      return (p2' :> NamedP n1 d1 p1')

-- |Commute a name patch and a named patch. In most cases this is
-- trivial but we do need to check explicit dependencies.
commuteNameNamed :: CommuteFn RebaseName (Named p)
commuteNameNamed (AddName an :> p@(NamedP pn deps _))
  | an == pn = error "impossible case"
  | an `elem` deps = Nothing
  | otherwise = Just (unsafeCoerceP p :> AddName an)
commuteNameNamed (DelName dn :> p@(NamedP pn deps _))
  -- this case can arise if a patch is suspended then a fresh copy is pulled from another repo
  | dn == pn = Nothing
  | dn `elem` deps = error "impossible case"
  | otherwise = Just (unsafeCoerceP p :> DelName dn)
commuteNameNamed (Rename old new :> NamedP pn deps body)
  | old == pn = error "impossible case"
  | new == pn = error "impossible case"
  | old `elem` deps = error "impossible case"
  | otherwise =
      let newdeps = map (\dep -> if new == dep then old else dep) deps
      in Just (NamedP pn newdeps (unsafeCoerceP body) :> Rename old new)

-- |Commute a named patch and a name patch. In most cases this is
-- trivial but we do need to check explicit dependencies.
commuteNamedName :: CommuteFn (Named p) RebaseName
commuteNamedName (p@(NamedP pn deps _) :> AddName an)
  | an == pn = error "impossible case"  -- the NamedP introduces pn, then AddName introduces it again
  | an `elem` deps = error "impossible case" -- the NamedP depends on an before it is introduced
  | otherwise = Just (AddName an :> unsafeCoerceP p)
commuteNamedName (p@(NamedP pn deps _) :> DelName dn)
  | dn == pn = Nothing
  | dn `elem` deps = Nothing
  | otherwise = Just (DelName dn :> unsafeCoerceP p)
commuteNamedName (NamedP pn deps body :> Rename old new)
  | old == pn = Nothing
  | new == pn = error "impossible case"
  | new `elem` deps = error "impossible case"
  | otherwise =
      let newdeps = map (\dep -> if old == dep then new else dep) deps
      in Just (Rename old new :> NamedP pn newdeps (unsafeCoerceP body))

canonizeNamePair :: (RebaseName :> RebaseName) wX wY -> FL RebaseName wX wY
canonizeNamePair (AddName n :> Rename old new) | n == old = AddName new :>: NilFL
canonizeNamePair (Rename old new :> DelName n) | n == new = DelName old :>: NilFL
canonizeNamePair (Rename old1 new1 :> Rename old2 new2) | new1 == old2 = Rename old1 new2 :>: NilFL
canonizeNamePair (n1 :> n2) = n1 :>: n2 :>: NilFL

pushFixupName :: PushFixupFn RebaseName RebaseName (FL RebaseName) (Maybe2 RebaseName)
pushFixupName (n1 :> n2)
 | IsEq <- isInverse = NilFL :> Nothing2
 | otherwise
   = case commute (n1 :> n2) of
       Nothing -> (canonizeNamePair (n1 :> n2)) :> Nothing2
       Just (n2' :> n1') -> (n2' :>: NilFL) :> Just2 n1'
  where isInverse = invert n1 =\/= n2
