module FreeType.LowLevel.Types where

import Foreign.C.Types

-- |Fixed point (26.6) number format:
-- 26 bits for integral, and 6 for fractional.
type F26Dot6 = CLong
