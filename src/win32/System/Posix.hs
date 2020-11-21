{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Posix ( sleep ) where

import Darcs.Prelude

import Foreign.C.Types ( CInt(..), CUInt(..), CULong(..) )

#include <windows_cconv.h>

foreign import WINDOWS_CCONV "winbase.h SleepEx" c_SleepEx :: CULong -> CUInt -> IO CInt

sleep :: Integer -> IO CInt
sleep n = c_SleepEx (1000 * fromIntegral n) 1
