module FreeType.Glyph
    ( module FreeType.LowLevel.Glyph
    , loadCharGlyph
    ) where

import FreeType.LowLevel.Face (Face, getCharIndex)
import FreeType.LowLevel.Glyph

-- |Load a glyph for a specific character.
loadCharGlyph :: Face -> Char -> [LoadFlags] -> IO (Maybe Glyph)
loadCharGlyph face c flags = do
    idx <- getCharIndex face c
    case idx of
        Nothing -> return Nothing
        Just i -> loadGlyph face i flags
