{-# LANGUAGE RecordWildCards #-}
module FreeType.LowLevel.GlyphMetrics where

import Foreign.Storable

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Wrapper for FT_Glyph_Metrics.
data GlyphMetrics a = GlyphMetrics
    { width :: a        -- ^The glyph's width.
    , height :: a       -- ^The glyph's height.
    , horiBearingX :: a -- ^Left side bearing for horizontal layout.
    , horiBearingY :: a -- ^Top side bearing for horizontal layout.
    , horiAdvance :: a  -- ^Advance width for horizontal layout.
    , vertBearingX :: a -- ^Left side bearing for vertical layout.
    , vertBearingY :: a -- ^Top side bearing for vertical layout.
                        -- Larger positive values mean further below the vertical glyph origin.
    , vertAdvance :: a  -- ^Advance height for vertical layout.
                        -- Positive values mean the glyph has a positive advance downward.
    } deriving (Eq, Show)

instance Functor GlyphMetrics where
    fmap f GlyphMetrics{..} = GlyphMetrics
        { width        = f width
        , height       = f height
        , horiBearingX = f horiBearingX
        , horiBearingY = f horiBearingY
        , horiAdvance  = f horiAdvance
        , vertBearingX = f vertBearingX
        , vertBearingY = f vertBearingY
        , vertAdvance  = f vertAdvance
        }

instance Storable a => Storable (GlyphMetrics a) where
    sizeOf _    = #size FT_Glyph_Metrics
    alignment _ = #alignment FT_Glyph_Metrics
    peek p = do
        width        <- (#peek FT_Glyph_Metrics, width) p
        height       <- (#peek FT_Glyph_Metrics, height) p
        horiBearingX <- (#peek FT_Glyph_Metrics, horiBearingX) p
        horiBearingY <- (#peek FT_Glyph_Metrics, horiBearingY) p
        horiAdvance  <- (#peek FT_Glyph_Metrics, horiAdvance) p
        vertBearingX <- (#peek FT_Glyph_Metrics, vertBearingX) p
        vertBearingY <- (#peek FT_Glyph_Metrics, vertBearingY) p
        vertAdvance  <- (#peek FT_Glyph_Metrics, vertAdvance) p
        return GlyphMetrics{..}
    poke p GlyphMetrics{..} = do
        (#poke FT_Glyph_Metrics, width)        p width
        (#poke FT_Glyph_Metrics, height)       p height
        (#poke FT_Glyph_Metrics, horiBearingX) p horiBearingX
        (#poke FT_Glyph_Metrics, horiBearingY) p horiBearingY
        (#poke FT_Glyph_Metrics, horiAdvance)  p horiAdvance
        (#poke FT_Glyph_Metrics, vertBearingX) p vertBearingX
        (#poke FT_Glyph_Metrics, vertBearingY) p vertBearingY
        (#poke FT_Glyph_Metrics, vertAdvance)  p vertAdvance
