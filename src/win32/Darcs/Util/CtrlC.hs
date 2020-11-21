{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Darcs.Util.CtrlC ( withCtrlCHandler ) where

import Darcs.Prelude

import Data.Word ( Word32 )
import Foreign.Ptr ( FunPtr )
import Control.Exception ( bracket_ )

#include <windows_cconv.h>

type Handler = Word32 -> IO Int

foreign import WINDOWS_CCONV "wrapper" wrap :: Handler -> IO (FunPtr Handler)
foreign import WINDOWS_CCONV "SetConsoleCtrlHandler" setConsoleCtrlHandler :: FunPtr Handler -> Int -> IO ()


withCtrlCHandler :: IO () -> IO a -> IO a
withCtrlCHandler handler m = do
    fp <- wrap (\_ctrlType -> handler >> return 1)
    bracket_ (setConsoleCtrlHandler fp 1) (setConsoleCtrlHandler fp 0) m



