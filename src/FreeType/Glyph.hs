module FreeType.Glyph
    ( module FreeType.LowLevel.Glyph
    , loadCharGlyph
    , withCharGlyph
    ) where

import FreeType.LowLevel.Face (Face, getCharIndex)
import FreeType.LowLevel.Glyph
import Control.Exception (bracket)

-- |Load a glyph for a specific character.
loadCharGlyph :: Face -> Char -> [LoadFlags] -> IO Glyph
loadCharGlyph face c flags = do
    idx <- getCharIndex face c
    loadGlyph face idx flags

-- |Load and discard a glyph for a specific character.
withCharGlyph :: Face -> Char -> [LoadFlags] -> (Glyph -> IO b) -> IO b
withCharGlyph face c flags = bracket (loadCharGlyph face c flags) doneGlyph
