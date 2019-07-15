module FreeType.Library
    ( Library
    , initFreeType, doneFreeType
    , withFreeType
    ) where

import FreeType.LowLevel.Library (Library, initFreeType, doneFreeType)

-- |Automatic initialization and finalization for FreeType.
withFreeType :: (Library -> IO a) -> IO a
withFreeType proc = do
    lib <- initFreeType
    res <- proc lib
    doneFreeType lib
    return res
