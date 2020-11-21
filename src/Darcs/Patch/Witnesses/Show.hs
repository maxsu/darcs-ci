module Darcs.Patch.Witnesses.Show
    ( Show1(..)
    , Show2(..)
    , show1
    , showsPrec1
    , show2
    , showsPrec2
    , showOp2
    , appPrec
    ) where

import Darcs.Prelude

import Darcs.Util.Show ( appPrec )

import Data.Constraint ( Dict(..) )

type ShowDict a = Dict (Show a)

showsPrecD :: ShowDict a -> Int -> a -> ShowS
showsPrecD Dict       = showsPrec

showD :: ShowDict a -> a -> String
showD Dict       = show

class Show1 a where
  showDict1 :: Dict (Show (a wX))
  default showDict1 :: Show (a wX) => ShowDict (a wX)
  showDict1 = Dict

showsPrec1 :: Show1 a => Int -> a wX -> ShowS
showsPrec1 = showsPrecD showDict1

show1 :: Show1 a => a wX -> String
show1 = showD showDict1

class Show2 a where
  showDict2 :: ShowDict (a wX wY)
  default showDict2 :: Show (a wX wY) => ShowDict (a wX wY)
  showDict2 = Dict

showsPrec2 :: Show2 a => Int -> a wX wY -> ShowS
showsPrec2 = showsPrecD showDict2

show2 :: Show2 a => a wX wY -> String
show2 = showD showDict2

showOp2 :: (Show2 a, Show2 b) => Int -> String -> Int -> a wW wX -> b wY wZ -> String -> String
showOp2 prec opstr d x y = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                          showString opstr . showsPrec2 (prec + 1) y
