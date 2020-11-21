module Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) ) where

import Darcs.Prelude

import Darcs.Util.Path ( AnchoredPath )

data SummDetail = SummAddDir AnchoredPath
                | SummRmDir  AnchoredPath
                | SummFile SummOp AnchoredPath Int Int Int
                | SummMv   AnchoredPath AnchoredPath
                | SummNone
  deriving (Ord, Eq)

data SummOp = SummAdd | SummRm | SummMod deriving (Ord, Eq)

