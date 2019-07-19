module FreeType.Library
    ( Library
    , initFreeType, doneFreeType
    , withFreeType
    ) where

import FreeType.LowLevel.Library (Library, initFreeType, doneFreeType)
import Control.Exception (bracket)

-- |Automatic initialization and finalization for FreeType.
withFreeType :: (Library -> IO a) -> IO a
withFreeType = bracket initFreeType doneFreeType
