{-# LANGUAGE MultiParamTypeClasses #-}
module Darcs.Test.TestOnly ( TestOnly ) where

-- |This nullary type class flags code that should only be used for
-- the tests. No instance of it should be defined in either the
-- darcs library or the main darcs executable.
class TestOnly
