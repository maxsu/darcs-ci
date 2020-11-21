-- Copyright (C) 2007 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}


module Darcs.Patch.V2.Non
    ( Non(..)
    , Nonable(..)
    , unNon
    , showNon
    , showNons
    , readNon
    , readNons
    , commutePrimsOrAddToCtx
    , commuteOrAddToCtx
    , commuteOrRemFromCtx
    , commuteOrAddToCtxRL
    , commuteOrRemFromCtxFL
    , remNons
    , (*>)
    , (>*)
    , (*>>)
    , (>>*)
    ) where

import Darcs.Prelude hiding ( (*>) )

import Data.List ( delete )
import Control.Monad ( liftM, mzero )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( commuteFL )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Invert ( Invert, invertFL, invertRL )
import Darcs.Patch.FromPrim
    ( FromPrim(..), ToFromPrim
    , PrimOf, PrimPatchBase, toPrim
    )
import Darcs.Patch.Prim ( sortCoalesceFL )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(invert) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Show ( showPatch )
import Darcs.Util.Parser ( Parser, lexChar )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), (+>+), mapRL_RL
    , (:>)(..), reverseFL, reverseRL )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, appPrec, showsPrec2 )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Patch.Read ( peekfor )
import Darcs.Patch.Show ( ShowPatchBasic, ShowPatchFor )
import Darcs.Patch.Viewing ()
import Darcs.Patch.Permutations ( removeFL, commuteWhatWeCanFL )
import Darcs.Util.Printer ( Doc, empty, vcat, hiddenPrefix, blueText, ($$) )
import qualified Data.ByteString.Char8 as BC ( pack, singleton )

-- |A 'Non' stores a context with a 'Prim' patch. It is a patch whose effect
-- isn't visible - a Non-affecting patch.
data Non p wX where
    Non :: FL p wX wY -> PrimOf p wY wZ -> Non p wX

-- |unNon converts a Non into a FL of its context followed by the primitive
-- patch.
unNon :: FromPrim p => Non p wX -> Sealed (FL p wX)
unNon (Non c x) = Sealed (c +>+ fromAnonymousPrim x :>: NilFL)

instance (Show2 p, Show2 (PrimOf p)) => Show (Non p wX) where
    showsPrec d (Non cs p) = showParen (d > appPrec) $ showString "Non " .
                             showsPrec2 (appPrec + 1) cs . showString " " .
                             showsPrec2 (appPrec + 1) p

instance (Show2 p, Show2 (PrimOf p)) => Show1 (Non p)

-- |showNons creates a Doc representing a list of Nons.
showNons :: (ShowPatchBasic p, PatchListFormat p, PrimPatchBase p)
         => ShowPatchFor -> [Non p wX] -> Doc
showNons _ [] = empty
showNons f xs = blueText "{{" $$ vcat (map (showNon f) xs) $$ blueText "}}"

-- |showNon creates a Doc representing a Non.
showNon :: (ShowPatchBasic p, PatchListFormat p, PrimPatchBase p)
        => ShowPatchFor
        -> Non p wX
        -> Doc
showNon f (Non c p) = hiddenPrefix "|" (showPatch f c)
                      $$ hiddenPrefix "|" (blueText ":")
                      $$ showPatch f p

-- |readNons is a parser that attempts to read a list of Nons.
readNons :: (ReadPatch p, PatchListFormat p, PrimPatchBase p)
         => Parser [Non p wX]
readNons = peekfor (BC.pack "{{") rns (return [])
  where rns = peekfor (BC.pack "}}") (return []) $
              do Sealed ps <- readPatch'
                 lexChar ':'
                 Sealed p <- readPatch'
                 (Non ps p :) `liftM` rns

-- |readNon is a parser that attempts to read a single Non.
readNon :: (ReadPatch p, PatchListFormat p, PrimPatchBase p)
        => Parser (Non p wX)
readNon = do Sealed ps <- readPatch'
             let doReadPrim = do Sealed p <- readPatch'
                                 return $ Non ps p
             peekfor (BC.singleton ':') doReadPrim mzero

-- |Nons are equal if their context patches are equal, and they have an equal
-- prim patch.
instance (Commute p, Eq2 p, Eq2 (PrimOf p)) => Eq (Non p wX) where
    Non (cx :: FL p wX wY1) (x :: PrimOf p wY1 wZ1)
     == Non (cy :: FL p wX wY2) (y :: PrimOf p wY2 wZ2) =
      case cx =\/= cy of
        IsEq -> case x =\/= y :: EqCheck wZ1 wZ2 of
                  IsEq -> True
                  NotEq -> False
        NotEq -> False

-- |Nonable represents the class of patches that can be turned into a Non.
class Nonable p where
    non :: p wX wY -> Non p wX

-- |'commuteOrAddToCtx' @x cy@ tries to commute @x@ past @cy@ and always
-- returns some variant @cy'@. If commutation suceeds, the variant is just
-- straightforwardly the commuted version. If commutation fails, the variant
-- consists of @x@ prepended to the context of @cy@.
commuteOrAddToCtx :: (Commute p, ToFromPrim p) => p wX wY -> Non p wY
                  -> Non p wX
commuteOrAddToCtx p n | Just n' <- p >* n = n'
commuteOrAddToCtx p (Non c x) = Non (p:>:c) x

-- | 'commuteOrAddToCtxRL' @xs cy@ commutes as many patches of @xs@ past @cy@
-- as possible, adding any that don't commute to the context of cy.  Suppose we
-- have
--
-- > x1 x2 x3 [c1 c2 y]
--
-- and that in our example @x1@ fails to commute past @c1@, this function
-- would commute down to
--
-- > x1 [c1'' c2'' y''] x2' x3'
--
-- and return @[x1 c1'' c2'' y'']@
commuteOrAddToCtxRL :: (Apply p, Commute p, Invert p, ToFromPrim p) => RL p wX wY -> Non p wY
                    -> Non p wX
commuteOrAddToCtxRL NilRL n = n
commuteOrAddToCtxRL (ps:<:p) n = commuteOrAddToCtxRL ps $ commuteOrAddToCtx p n

-- |abstract over 'FL'/'RL'
class WL l where
   toRL :: l p wX wY -> RL p wX wY
   invertWL :: Invert p => l p wX wY -> l p wY wX

instance WL FL where
   toRL = reverseFL
   invertWL = reverseRL . invertFL

instance WL RL where
   toRL = id
   invertWL = reverseFL . invertRL

-- |commutePrimsOrAddToCtx takes a WL of prims and attempts to commute them
-- past a Non.
commutePrimsOrAddToCtx :: (WL l, Apply p, Commute p, Invert p, ToFromPrim p) => l (PrimOf p) wX wY
         -> Non p wY -> Non p wX
commutePrimsOrAddToCtx q = commuteOrAddToCtxRL (mapRL_RL fromAnonymousPrim $ toRL q)

-- TODO: Figure out what remNons is for; it's is only used in one place - when
-- commuting two Conflictors:
--
-- > commute (Conflictor a1 n1 p1 :> Conflictor a2 n2 p2)
-- > ...
-- >   a1' = map (commutePrimsOrAddToCtx n2) a1
-- >   p2ooo = remNons a1' p2
-- >   n2n1 = n2 +>+ n1
-- > n1' :> n2' <- return $ filterConflictsFL p2ooo n2n1
--
-- which appears to be munging the not-yet-undone FLs in the Conflictors. a1'
-- will be the list of Nons with n2 commuted in/past them. So we then want to
-- modify p2, so that it doesn't have any of a1' in its context.

-- remNons really only works right if the relevant nons are conflicting...
remNons :: (Nonable p, Effect p, Apply p, Commute p, Invert p, Eq2 p, ToFromPrim p, PrimPatchBase p)
        => [Non p wX] -> Non p wX -> Non p wX
remNons ns n@(Non c x) = case remNonHelper ns c of
                             NilFL :> c' -> Non c' x
                             _ -> n
  where
    remNonHelper :: (Nonable p, Effect p, Apply p, Commute p, Invert p, Eq2 p, ToFromPrim p,
                 PrimPatchBase p) => [Non p wX]
                 -> FL p wX wY -> (FL (PrimOf p) :> FL p) wX wY
    remNonHelper [] x = NilFL :> x
    remNonHelper _ NilFL = NilFL :> NilFL
    remNonHelper ns (c:>:cs)
        | non c `elem` ns =
          let nsWithoutC = delete (non c) ns in
          let commuteOrAddInvC = commuteOrAddToCtx $ invert c in
          case remNonHelper (map commuteOrAddInvC nsWithoutC) cs of
              a :> z -> sortCoalesceFL (effect c +>+ a) :> z
        | otherwise = case commuteWhatWeCanFL (c :> cs) of
                          b :> c' :> d -> case remNonHelper ns b of
                              a :> b' -> a :> (b' +>+ c' :>: d)

-- |commuteOrRemFromCtx attempts to remove a given patch from a Non. If the
-- patch was not in the Non, then the commute will succeed and the modified Non
-- will be returned. If the commute fails then the patch is either in the Non
-- context, or the Non patch itself; we attempt to remove the patch from the
-- context and then return the non with the updated context.
--
-- TODO: understand if there is any case where p is equal to the prim patch of
-- the Non, in which case, we return the original Non, is that right?
commuteOrRemFromCtx :: (Commute p, Invert p, Eq2 p, ToFromPrim p) => p wX wY -> Non p wX
     -> Maybe (Non p wY)
commuteOrRemFromCtx p n | n'@(Just _) <- n *> p = n'
commuteOrRemFromCtx p (Non pc x) = removeFL p pc >>= \c -> return (Non c x)

-- |commuteOrRemFromCtxFL attempts to remove a FL of patches from a Non,
-- returning Nothing if any of the individual removes fail.
commuteOrRemFromCtxFL :: (Apply p, Commute p, Invert p, Eq2 p, ToFromPrim p) => FL p wX wY -> Non p wX
                      -> Maybe (Non p wY)
commuteOrRemFromCtxFL NilFL n = Just n
commuteOrRemFromCtxFL (p:>:ps) n = do n' <- commuteOrRemFromCtx p n
                                      commuteOrRemFromCtxFL ps n'

-- |(*>) attemts to modify a Non by commuting it past a given patch.
(*>) :: (Commute p, Invert p, ToFromPrim p) => Non p wX -> p wX wY
     -> Maybe (Non p wY)
n *> p = invert p >* n

-- |(>*) attempts to modify a Non, by commuting a given patch past it.
(>*) :: (Commute p, ToFromPrim p) => p wX wY -> Non p wY
     -> Maybe (Non p wX)
y >* (Non c x) = do
    c' :> y' <- commuteFL (y :> c)
    px' :> _ <- commute (y' :> fromAnonymousPrim x)
    x' <- toPrim px'
    return (Non c' x')

-- |(*>>) attempts to modify a Non by commuting it past a given WL of patches.
(*>>) :: (WL l, Apply p, Commute p, Invert p, ToFromPrim p, PrimPatchBase p) => Non p wX
      -> l (PrimOf p) wX wY -> Maybe (Non p wY)
n *>> p = invertWL p >>* n

-- |(>>*) attempts to modify a Non by commuting a given WL of patches past it.
(>>*) :: (WL l, Apply p, Commute p, Invert p, ToFromPrim p) => l (PrimOf p) wX wY -> Non p wY
      -> Maybe (Non p wX)
ps >>* n = commuteRLPastNon (toRL ps) n
  where
    commuteRLPastNon :: (Apply p, Commute p, Invert p, ToFromPrim p) => RL (PrimOf p) wX wY
                     -> Non p wY -> Maybe (Non p wX)
    commuteRLPastNon NilRL n = Just n
    commuteRLPastNon (xs:<:x) n = fromAnonymousPrim x >* n >>= commuteRLPastNon xs
