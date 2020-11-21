module Darcs.UI.Options
    ( module Darcs.UI.Options.Core
    , DarcsOption
    , PrimDarcsOption
    , DarcsOptDescr
    , optDescr
    , Config
    ) where

import Darcs.Prelude

import Data.Functor.Compose ( getCompose )
import System.Console.GetOpt ( OptDescr )

import Darcs.UI.Options.All ( DarcsOption )
import Darcs.UI.Options.Core
import Darcs.UI.Options.Util ( DarcsOptDescr, Flag, PrimDarcsOption )
import Darcs.Util.Path ( AbsolutePath )

-- | Instantiate a 'DarcsOptDescr' with an 'AbsolutePath'
optDescr :: AbsolutePath -> DarcsOptDescr f -> OptDescr f
optDescr path = fmap ($ path) . getCompose

type Config = [Flag]
