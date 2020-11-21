module Darcs.Patch.Witnesses.Unsafe
    ( unsafeCoerceP
    , unsafeCoercePStart
    , unsafeCoercePEnd
    , unsafeCoerceP1
    ) where

import Unsafe.Coerce

unsafeCoerceP :: a wX wY -> a wB wC
unsafeCoerceP = unsafeCoerce

unsafeCoercePStart :: a wX1 wY -> a wX2 wY
unsafeCoercePStart = unsafeCoerce

unsafeCoercePEnd :: a wX wY1 -> a wX wY2
unsafeCoercePEnd = unsafeCoerce

unsafeCoerceP1 :: a wX -> a wY
unsafeCoerceP1 = unsafeCoerce
