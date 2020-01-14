module FreeType.Utils where

import FreeType.Face
import FreeType.Glyph
import FreeType.Outline hiding (c_outline)
import FreeType.LowLevel.GlyphSlot
import FreeType.LowLevel.GlyphMetrics
import FreeType.LowLevel.Types

import FreeType.Error

import Foreign.Ptr
import Foreign.Storable

-- |Extract Bezier curve outline and glyph metrics for a character.
-- Note that at one time only 1 'POutline' can be valid.
-- The last returned 'POutline' is invalidated at next call.
-- Some other functions may also invalidate this 'POutline'.
loadOutlineAndMetrics :: RealFrac a => Face -> Char -> IO (POutline, GlyphMetrics a)
loadOutlineAndMetrics face ch = do
    idx <- getCharIndex face ch
    loadGlyph face idx [LoadNoBitmap]
    slot <- peek (c_glyph face)
    let po = c_outline slot
    assert (po /= nullPtr) "Not an outline glyph."
    metrics <- peek (c_metrics slot) :: IO (GlyphMetrics F26'6)
    return (po, fromFixedPoint <$> metrics)
