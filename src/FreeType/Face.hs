module FreeType.Face
    ( module FreeType.LowLevel.Face
    , withFace
    ) where

import FreeType.Library
import FreeType.LowLevel.Face

-- |Automatic load and discard font face.
withFace :: Library -> String -> Int -> (Face -> IO a) -> IO a
withFace lib path idx proc = do
    fc <- newFace lib path idx
    res <- proc fc
    doneFace fc
    return res
