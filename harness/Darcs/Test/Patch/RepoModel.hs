module Darcs.Test.Patch.RepoModel where

import Darcs.Prelude

import Darcs.Patch.Apply ( Apply, ApplyState )
import Darcs.Patch.Witnesses.Ordered ( FL, RL )

import Test.QuickCheck ( Gen )

data Fail a = Failed String | OK a
  deriving (Eq, Show)

instance Functor Fail where
  fmap _ (Failed s) = Failed s
  fmap f (OK v) = OK (f v)

instance Applicative Fail where
  pure = OK
  Failed s <*> _ = Failed s
  _ <*> Failed s = Failed s
  OK f <*> OK v = OK (f v)

instance Monad Fail where
  return = OK
  Failed s >>= _ = Failed s
  OK v >>= f = f v

unFail :: Fail t -> t
unFail (OK x) = x
unFail (Failed err) = error $ "unFail failed: " ++ err

maybeFail :: Fail a -> Maybe a
maybeFail (OK x) = Just x
maybeFail _ = Nothing

class RepoModel model where
  type RepoState model :: (* -> *) -> *
  showModel :: model x -> String
  eqModel :: model x -> model x -> Bool
  aSmallRepo :: Gen (model x)
  repoApply :: (Apply p, ApplyState p ~ RepoState model) => model x -> p x y -> Fail (model y)

type family ModelOf (p :: * -> * -> *) :: * -> *

type instance ModelOf (FL p) = ModelOf p
type instance ModelOf (RL p) = ModelOf p
