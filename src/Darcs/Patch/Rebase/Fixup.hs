--  Copyright (C) 2011-2 Ganesh Sittampalam 
--
--  BSD3

{-# LANGUAGE UndecidableInstances #-}
module Darcs.Patch.Rebase.Fixup
    ( RebaseFixup(..)
    , commuteNamedFixup, commuteFixupNamed
    , pushFixupFixup
    , flToNamesPrims, namedToFixups
    ) where

import Darcs.Prelude

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..), selfCommuter )
import Darcs.Patch.CommuteFn ( totalCommuterIdFL )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Named ( Named(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.FromPrim ( PrimPatchBase(..) )
import Darcs.Patch.Prim ( PrimPatch, canonizeFL )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Rebase.Name
    ( RebaseName(..)
    , commuteNamedName, commuteNameNamed
    , commuterNamedId, commuterIdNamed
    , commutePrimName, commuteNamePrim
    , pushFixupName
    )
import Darcs.Patch.Rebase.PushFixup ( PushFixupFn )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Witnesses.Eq ( Eq2(..), EqCheck(..) )
import Darcs.Patch.Witnesses.Maybe ( Maybe2(..), mapMB_MB )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), mapFL_FL, (:>)(..) )
import Darcs.Patch.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Patch.Witnesses.Show ( Show1, Show2, showsPrec2, appPrec )

import qualified Darcs.Util.Diff as D ( DiffAlgorithm )
import Darcs.Util.Parser ( Parser, lexString )
import Darcs.Util.Printer ( ($$), (<+>), blueText )

import Control.Applicative ( (<|>) )
import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

-- |A single rebase fixup, needed to ensure that the actual patches
-- being stored in the rebase state have the correct context.
data RebaseFixup prim wX wY where
  PrimFixup :: prim wX wY -> RebaseFixup prim wX wY
  NameFixup :: RebaseName wX wY -> RebaseFixup prim wX wY

namedToFixups :: (PrimPatch (PrimOf p), Effect p) => Named p wX wY -> FL (RebaseFixup (PrimOf p)) wX wY
namedToFixups (NamedP p _ contents) = NameFixup (AddName p) :>: mapFL_FL PrimFixup (effect contents)

instance Show2 prim => Show (RebaseFixup prim wX wY) where
    showsPrec d (PrimFixup p) =
        showParen (d > appPrec) $ showString "PrimFixup " . showsPrec2 (appPrec + 1) p
    showsPrec d (NameFixup p) =
        showParen (d > appPrec) $ showString "NameFixup " . showsPrec2 (appPrec + 1) p

instance Show2 prim => Show1 (RebaseFixup prim wX)

instance Show2 prim => Show2 (RebaseFixup prim)

instance PrimPatch prim => PrimPatchBase (RebaseFixup prim) where
    type PrimOf (RebaseFixup prim) = prim

instance Apply prim => Apply (RebaseFixup prim) where
    type ApplyState (RebaseFixup prim) = ApplyState prim
    apply (PrimFixup p) = apply p
    apply (NameFixup _) = return ()
    unapply (PrimFixup p) = unapply p
    unapply (NameFixup _) = return ()

instance Invert prim => Invert (RebaseFixup prim) where
    invert (PrimFixup p) = PrimFixup (invert p)
    invert (NameFixup n) = NameFixup (invert n)

instance PatchInspect prim => PatchInspect (RebaseFixup prim) where
    listTouchedFiles (PrimFixup p) = listTouchedFiles p
    listTouchedFiles (NameFixup n) = listTouchedFiles n

    hunkMatches f (PrimFixup p) = hunkMatches f p
    hunkMatches f (NameFixup n) = hunkMatches f n

instance PatchListFormat (RebaseFixup prim)

instance ShowPatchBasic prim => ShowPatchBasic (RebaseFixup prim) where
  showPatch f (PrimFixup p) =
    blueText "rebase-fixup" <+> blueText "(" $$ showPatch f p $$ blueText ")"
  showPatch f (NameFixup p) =
    blueText "rebase-name" <+> blueText "(" $$ showPatch f p $$ blueText ")"

instance ReadPatch prim => ReadPatch (RebaseFixup prim) where
 readPatch' =
   mapSeal PrimFixup <$> readWith (BC.pack "rebase-fixup" ) <|>
   mapSeal NameFixup <$> readWith (BC.pack "rebase-name"  )
   where
     readWith :: forall q wX . ReadPatch q => B.ByteString -> Parser (Sealed (q wX))
     readWith str = do
       lexString str
       lexString (BC.pack "(")
       res <- readPatch'
       lexString (BC.pack ")")
       return res


instance Commute prim => Commute (RebaseFixup prim) where
    commute (PrimFixup p :> PrimFixup q) = do
        q' :> p' <- commute (p :> q)
        return (PrimFixup q' :> PrimFixup p')

    commute (NameFixup p :> NameFixup q) = do
        q' :> p' <- commute (p :> q)
        return (NameFixup q' :> NameFixup p')

    commute (PrimFixup p :> NameFixup q) = do
        q' :> p' <- return $ commutePrimName (p :> q)
        return (NameFixup q' :> PrimFixup p')

    commute (NameFixup p :> PrimFixup q) = do
        q' :> p' <- return $ commuteNamePrim (p :> q)
        return (PrimFixup q' :> NameFixup p')

pushFixupPrim
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> PushFixupFn prim prim (FL prim) (Maybe2 prim)
pushFixupPrim da (f1 :> f2)
 | IsEq <- isInverse = NilFL :> Nothing2
 | otherwise
   = case commute (f1 :> f2) of
       Nothing -> canonizeFL da (f1 :>: f2 :>: NilFL) :> Nothing2
       Just (f2' :> f1') -> (f2' :>: NilFL) :> Just2 f1'
  where isInverse = invert f1 =\/= f2

pushFixupFixup
  :: PrimPatch prim
  => D.DiffAlgorithm
  -> PushFixupFn
       (RebaseFixup prim) (RebaseFixup prim)
       (FL (RebaseFixup prim)) (Maybe2 (RebaseFixup prim))

pushFixupFixup da (PrimFixup f1 :> PrimFixup f2)
  = case pushFixupPrim da (f1 :> f2) of
      fs2' :> f1' -> mapFL_FL PrimFixup fs2' :> mapMB_MB PrimFixup f1'

pushFixupFixup _da (PrimFixup f :> NameFixup n)
  = case commutePrimName (f :> n) of
      n' :> f' -> (NameFixup n' :>: NilFL) :> Just2 (PrimFixup f')

pushFixupFixup _da (NameFixup n1 :> NameFixup n2)
  = case pushFixupName (n1 :> n2) of
      ns2' :> n1' -> mapFL_FL NameFixup ns2' :> mapMB_MB NameFixup n1'

pushFixupFixup _da (NameFixup n :> PrimFixup f)
  = case commuteNamePrim (n :> f) of
      f' :> n' -> (PrimFixup f' :>: NilFL) :> Just2 (NameFixup n')


-- |Split a sequence of fixups into names and prims
flToNamesPrims :: FL (RebaseFixup prim) wX wY
               -> (FL RebaseName :> FL prim) wX wY
flToNamesPrims NilFL = NilFL :> NilFL
flToNamesPrims (NameFixup n :>: fs) =
    case flToNamesPrims fs of
        names :> prims -> (n :>: names) :> prims
flToNamesPrims (PrimFixup p :>: fs) =
    case flToNamesPrims fs of
        names :> prims ->
            case totalCommuterIdFL commutePrimName (p :> names) of
                names' :> p' -> names' :> (p' :>: prims)

commuteNamedFixup
  :: Commute prim
  => (Named prim :> RebaseFixup prim) wX wY
  -> Maybe ((RebaseFixup prim :> Named prim) wX wY)
commuteNamedFixup (p :> PrimFixup q) = do
    q' :> p' <- commuterNamedId selfCommuter (p :> q)
    return (PrimFixup q' :> p')
commuteNamedFixup (p :> NameFixup n) = do
    n' :> p' <- commuteNamedName (p :> n)
    return (NameFixup n' :> p')

commuteFixupNamed
  :: Commute prim
  => (RebaseFixup prim :> Named prim) wX wY
  -> Maybe ((Named prim :> RebaseFixup prim) wX wY)
commuteFixupNamed (PrimFixup p :> q) = do
    q' :> p' <- commuterIdNamed selfCommuter (p :> q)
    return (q' :> PrimFixup p')
commuteFixupNamed (NameFixup n :> q) = do
    q' :> n' <- commuteNameNamed (n :> q)
    return (q' :> NameFixup n')
