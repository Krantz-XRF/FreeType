module FreeType.Face
    ( module FreeType.LowLevel.Face
    , withFace
    ) where

import FreeType.Library
import FreeType.LowLevel.Face
import Control.Exception (bracket)

-- |Automatic load and discard font face.
withFace :: Library -> String -> Int -> (Face -> IO a) -> IO a
withFace lib path idx = bracket (newFace lib path idx) doneFace
