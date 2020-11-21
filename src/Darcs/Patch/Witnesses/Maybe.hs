module Darcs.Patch.Witnesses.Maybe
  ( Maybe2(..)
  , maybeToFL, maybeToRL
  , mapMB_MB
  ) where

import Darcs.Patch.Witnesses.Ordered ( FL(..), RL(..) )

data Maybe2 p wX wY where
  Nothing2 :: Maybe2 p wX wX
  Just2 :: p wX wY -> Maybe2 p wX wY

maybeToFL :: Maybe2 p wX wY -> FL p wX wY
maybeToFL Nothing2 = NilFL
maybeToFL (Just2 v) = v :>: NilFL

maybeToRL :: Maybe2 p wX wY -> RL p wX wY
maybeToRL Nothing2 = NilRL
maybeToRL (Just2 v) = NilRL :<: v

mapMB_MB :: (p wX wY -> q wX wY) -> Maybe2 p wX wY -> Maybe2 q wX wY
mapMB_MB _ Nothing2 = Nothing2
mapMB_MB f (Just2 v) = Just2 (f v)