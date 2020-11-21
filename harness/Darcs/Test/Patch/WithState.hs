{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.WithState where

import Darcs.Prelude

import Darcs.Patch.Apply
import Darcs.Patch.Commute
import Darcs.Patch.Effect
import Darcs.Patch.FromPrim
import Darcs.Patch.Invert
import Darcs.Patch.Prim.Class
import Darcs.Patch.Witnesses.Eq
import Darcs.Patch.Witnesses.Maybe
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Show
import Test.QuickCheck ( Gen, Arbitrary(..), sized, choose )

import Darcs.Test.Patch.RepoModel
import Darcs.Test.Patch.Arbitrary.Shrink

import Data.Maybe

----------------------------------------------------------------------
-- * WithState

data WithState p wX wY = WithState {
                              wsStartState :: (ModelOf p) wX
                            , wsPatch      :: p wX wY
                            , wsEndState   :: (ModelOf p) wY
                            }

type instance ModelOf (WithState p) = ModelOf p

instance (Show1 (ModelOf p), Show2 p) => Show (WithState p wX wY) where
  showsPrec d (WithState s p s')
    = showParen (d > appPrec) $ showString "WithState "
                              . showsPrec1 (appPrec+1) s
                              . showString " "
                              . showsPrec2 (appPrec+1) p
                              . showString " "
                              . showsPrec1 (appPrec+1) s'

instance (Show1 (ModelOf p), Show2 p) => Show1 (WithState p wA)

instance (Show1 (ModelOf p), Show2 p) => Show2 (WithState p)

-- | This is only used for the legacy 'Tree' based test generator, where the
-- @p@ parameter gets instantiated to @'Tree' p@ (which has no definite end
-- state).
data WithStartState s p wX = WithStartState {
                                 wssStartState :: s wX
                               , wssPatch      :: p wX
                               }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithStartState s p wX) where
   showsPrec d (WithStartState s p) = showParen (d > appPrec) $ showString "WithStartState " .
                                      showsPrec1 (appPrec + 1) s . showString " " .
                                      showsPrec1 (appPrec + 1) p

instance (Show1 s, Show1 p) => Show1 (WithStartState s p)

-- |'WithStartState2' is like 'WithStartState' but for patches that have both witnesses.
data WithStartState2 p wX wY =
  WithStartState2
  { wss2StartState :: ModelOf p wX
  , wss2Patch      :: p wX wY
  }

instance (Show1 (ModelOf p), Show2 p) => Show (WithStartState2 p wX wY) where
  showsPrec d (WithStartState2 s p) =
    showParen (d > appPrec) $ showString "WithStartState2 " .
    showsPrec1 (appPrec + 1) s . showString " " .
    showsPrec2 (appPrec + 1) p

instance (Show1 (ModelOf p), Show2 p) => Show1 (WithStartState2 p wX)
instance (Show1 (ModelOf p), Show2 p) => Show2 (WithStartState2 p)

-- | A combination of a patch and its final state. The state, in this module, is
--   typically represented by a 'RepoModel' value. The @px@ type is typically a
--   patch type applied to its pre-state, e.g. @Prim x@.
data WithEndState s px wY = WithEndState {
                                wesPatch    :: px wY
                              , wesEndState :: s wY
                              }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithEndState s p wX) where
   showsPrec d (WithEndState p s) = showParen (d > appPrec) $ showString "WithEndState " .
                                    showsPrec1 (appPrec + 1) p . showString " " .
                                    showsPrec1 (appPrec + 1) s


instance (Show1 s, Show1 p) => Show1 (WithEndState s p)


----------------------------------------------------------------------
-- * ArbitraryState generators

-- | A type class to generate arbitrary values, threading a state through the
--   arbitrary calls. So this can be used to generate a patch that comes after
--   another patch. The post-state of the generated patch is hidden by the
--   'Sealed'.
class ArbitraryState p where
  arbitraryState :: ModelOf p wX -> Gen (Sealed (WithEndState (ModelOf p) (p wX)))

instance ArbitraryState p => ArbitraryState (WithState p) where
  arbitraryState s = do Sealed (WithEndState x s') <- arbitraryState s
                        return $ seal $ WithEndState (WithState s x s') s'

type instance ModelOf (p :> p) = ModelOf p

instance ArbitraryState p => ArbitraryState (p :> p) where
  arbitraryState s = do Sealed (WithEndState p1 s') <- arbitraryState s
                        Sealed (WithEndState p2 s'') <- arbitraryState s'
                        return $ seal $ WithEndState (p1 :> p2) s'' 

{- the type instance overlaps!
type instance ModelOf (p :> p :> p) = ModelOf p

instance ArbitraryState p => ArbitraryState (p :> p :> p) where
-}

arbitraryTriple :: ArbitraryState p
                => ModelOf p wX
                -> Gen (Sealed (WithEndState (ModelOf p) ((p :> p :> p) wX)))
arbitraryTriple s = do
  Sealed (WithEndState p1 s') <- arbitraryState s
  Sealed (WithEndState p2 s'') <- arbitraryState s'
  Sealed (WithEndState p3 s''') <- arbitraryState s''
  return $ seal $ WithEndState (p1 :> p2 :> p3) s'''

arbitraryFL ::
     ArbitraryState p
  => forall wX. Int -> ModelOf p wX -> Gen (Sealed (WithEndState (ModelOf p) (FL p wX)))
arbitraryFL 0 s = return $ seal $ WithEndState NilFL s
arbitraryFL n s = do Sealed (WithEndState x s') <- arbitraryState s
                     Sealed (WithEndState xs s'') <- arbitraryFL (n-1) s'
                     return $ seal $ WithEndState (x :>: xs) s''

instance ArbitraryState p => ArbitraryState (FL p) where
  arbitraryState s = sized $ \n -> do k <- choose (0, min 2 (n `div` 5))
                                      arbitraryFL k s


makeS2Gen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed2 p)
makeS2Gen stGen = do s <- stGen
                     Sealed (WithEndState p _) <- arbitraryState s
                     return $ seal2 p

makeSGen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed (p wX))
makeSGen stGen = do s <- stGen
                    Sealed (WithEndState p _) <- arbitraryState s
                    return $ seal p

makeWS2Gen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed2 (WithState p))
makeWS2Gen stGen = do s <- stGen
                      Sealed (WithEndState wsP _) <- arbitraryState s
                      return $ seal2 wsP

makeWSGen :: ArbitraryState p => Gen (ModelOf p wX) -> Gen (Sealed (WithState p wX))
makeWSGen stGen = do s <- stGen
                     Sealed (WithEndState wsP _) <- arbitraryState s
                     return $ seal wsP

-- | A class to help with shrinking complex test cases by simplifying
-- the starting state of the test case. See also 'PropagateShrink'.
class ShrinkModel prim where
  -- |Given a repository state, produce a patch that simplifies the
  -- repository state. The inverse of the patch can be passed as the
  -- "shrinking fixup" to 'propagateShrink'.
  --
  -- Imagine that we start with
  --
  --    s wX1 --p1 wX1 wY1--> s wY1
  --
  -- If we shrink the state to @s wX2@:
  --
  --    s wX2 <--prim wX1 wX2-- s wX1
  --
  -- then we hope that 'propagateShrink' will produce a simpler version of @p1@,
  -- @p2@, that starts from the simpler state @s wX2@:
  --
  --                        p2 wX2 wY2
  --               s wX2 ----------------> s wY2
  --                |                        |
  --                |                        |
  --    invert prim |                        | (discard)
  --                |                        |
  --                V                        V
  --               s wX1 ----------------> s wY1
  --                        p1 wX1 wY1
  shrinkModelPatch :: ModelOf prim wX -> [Sealed (prim wX)]

checkOK :: Fail a -> [a]
checkOK (OK a) = [a]
checkOK (Failed _) = []

shrinkModel
  :: forall s prim wX
   . (Apply prim, ApplyState prim ~ RepoState s, ModelOf prim ~ s, RepoModel s, ShrinkModel prim)
  => s wX -> [Sealed (WithEndState s (prim wX))]
shrinkModel s = do
  Sealed prim <- shrinkModelPatch s
  endState <- checkOK $ repoApply s prim
  return $ Sealed $ WithEndState prim endState

-- | A class to help with shrinking complex test cases. The idea is that the
-- "starting state" of the test case is shrunk and this results in a "fixup"
-- primitive that goes from the shrunk starting state to the original starting
-- state. This so-called "shrinking fixup" is then propagated through the test
-- case to generate a new test case that starts at the shrunk starting state.
-- The shrinking fixup is typically generated via the 'ShrinkModel' class.
class PropagateShrink prim p where
  -- Given a test patch (of type @p@) and a shrinking fixup (of type @prim@),
  -- try to propagate the shrinking fixup past the test patch.
  -- The @Maybe2 p@ return type allows the fixup to eliminate the shrinking
  -- patch entirely, and vice versa the @Maybe2 prim@ allows the shrinking fixup
  -- to disappear (for example it might be cancelled out by something in the test
  -- patch).
  -- We don't use @FL p@, because that would only really be useful for a "stuck"
  -- fixup - one that doesn't eliminate or commute - and that implies that
  -- the state shrink isn't actually shrinking the real test case.
  propagateShrink :: (prim :> p) wX wY -> Maybe ((Maybe2 p :> Maybe2 prim) wX wY)

propagateShrinkKeep
  :: PropagateShrink prim p
  => (prim :> p) wX wY
  -> Maybe ((p :> Maybe2 prim) wX wY)
propagateShrinkKeep inp = do
  Just2 p' :> mprim' <- propagateShrink inp
  return (p' :> mprim')

propagateShrinkMaybe
  :: PropagateShrink prim p
  => (Maybe2 prim :> p) wX wY
  -> Maybe ((Maybe2 p :> Maybe2 prim) wX wY)
propagateShrinkMaybe (Nothing2 :> p) = Just (Just2 p :> Nothing2)
propagateShrinkMaybe (Just2 prim :> p) = propagateShrink (prim :> p)

-- |Shrink a test case wrapped with 'WithStartState2' by shrinking the start state
-- of the test case with 'ShrinkModel' and then propagating the shrink through the
-- patch type of the test case.
shrinkState
  :: forall s prim p
   . ( Invert prim, Apply prim, RepoModel s
     , ShrinkModel prim, PropagateShrink prim p
     , ApplyState prim ~ RepoState s
     , ModelOf p ~ s
     , ModelOf prim ~ s
     )
  => Sealed2 (WithStartState2 p)
  -> [Sealed2 (WithStartState2 p)]
shrinkState (Sealed2 (WithStartState2 s p)) = do
  Sealed (WithEndState fixup shrunkState) <- shrinkModel @s @prim s
  p' :> _ <- maybeToList $ propagateShrinkKeep (invert fixup :> p)
  return $ Sealed2 $ WithStartState2 shrunkState p'

shrinkAtStartState
  :: ( Shrinkable p, RepoModel (ModelOf p), Effect p
     , prim ~ PrimOf p, Invert prim, Apply prim
     , ApplyState prim ~ RepoState (ModelOf p)
     )
  => WithStartState2 p wX wY
  -> [FlippedSeal (WithStartState2 p) wY]
shrinkAtStartState (WithStartState2 s p) = do
  FlippedSeal p' <- shrinkAtStart p
  endState <- checkOK $ repoApply s (effect p)
  newState <- checkOK $ repoApply endState (invert (effect p'))
  return $ FlippedSeal (WithStartState2 newState p')

instance
  ( ArbitraryState p, Shrinkable p, RepoModel s
  , s ~ ModelOf p
  , s ~ ModelOf prim
  , Effect p
  , Apply prim, ApplyState prim ~ RepoState s
  , prim ~ PrimOf p, Invert prim, ShrinkModel prim, PropagateShrink prim p
  )
  => Arbitrary (Sealed2 (WithStartState2 p)) where
  arbitrary = do
    repo <- aSmallRepo @s
    Sealed (WithEndState p _) <- arbitraryState repo
    return (Sealed2 (WithStartState2 repo p))
  shrink w@(Sealed2 (WithStartState2 repo p)) =
    map (Sealed2 . WithStartState2 repo) (shrinkInternally p) ++
    map (unseal (Sealed2 . WithStartState2 repo)) (shrinkAtEnd p) ++
    map (unsealFlipped Sealed2) (shrinkAtStartState (WithStartState2 repo p)) ++
    shrinkState @s @prim @p w

propagatePrim
  :: (Eq2 prim, PrimCanonize prim, Invert prim, Commute prim)
  => (prim :> prim) wX wY -> Maybe ((Maybe2 prim :> Maybe2 prim) wX wY)
propagatePrim (p1 :> p2)
  | IsEq <- invert p1 =\/= p2 = Just (Nothing2 :> Nothing2)
  | Just (p2' :> p1') <- commute (p1 :> p2) = Just (Just2 p2' :> Just2 p1')
  | Just p' <- primCoalesce p1 p2 = Just (Just2 p' :> Nothing2)
  | otherwise = Nothing

instance (PropagateShrink prim p, PropagateShrink prim q)
  => PropagateShrink prim (p :> q) where

  propagateShrink (prim :> (p :> q)) = do
    Just2 mp' :> mprim' <- propagateShrink (prim :> p)
    Just2 mq' :> mprim'' <- propagateShrinkMaybe (mprim' :> q)
    return (Just2 (mp' :> mq') :> mprim'')

instance PropagateShrink prim p => PropagateShrink prim (FL p) where
  propagateShrink (prim :> NilFL) = Just (Just2 NilFL :> Just2 prim)
  propagateShrink (prim :> (p :>: ps)) = do
    mp' :> mprim' <- propagateShrink (prim :> p)
    Just2 ps' :> mprim'' <- propagateShrinkMaybe (mprim' :> ps)
    let result = case mp' of
          Nothing2 -> ps'
          Just2 p' -> p' :>: ps'
    return (Just2 result :> mprim'')
