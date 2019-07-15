{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
module FreeType.LowLevel.Glyph
    ( LoadFlags
        ( ..
        , LoadDefault
        , LoadNoScale
        , LoadNoHinting
        , LoadRender
        , LoadNoBitmap
        , LoadVerticalLayout
        , LoadForceAutohint
        , LoadCropBitmap
        , LoadPedantic
        , LoadIgnoreGlobalAdvanceWidth
        , LoadNoRecurse
        , LoadIgnoreTransform
        , LoadMonochrome
        , LoadLinearDesign
        , LoadNoAutohint
        , LoadColor
        , LoadComputeMetrics
        , LoadBitmapMetricsOnly
        , LoadTargetNormal
        , LoadTargetLight
        , LoadTargetMono
        , LoadTargetLcd
        , LoadTargetLcdV
        )
    , GlyphFormat
        ( GlyphFormatComposite
        , GlyphFormatBitmap
        , GlyphFormatOutline
        , GlyphFormatPlotter
        )
    , GlyphRec, Glyph
    , loadGlyph, doneGlyph
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.Bits ((.|.))
import Data.List (foldl')

import FreeType.LowLevel.Face (Face, c_glyph)
import FreeType.LowLevel.GlyphSlot (GlyphSlot)
import FreeType.Error (ErrorCode(..), unwrapError, isError)

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

-- |FT_LOAD_* and FT_LOAD_TARGET_* flags
newtype LoadFlags = LoadFlags { unwrapLoadFlags :: CInt } deriving (Show)
pattern LoadDefault           = LoadFlags (#const FT_LOAD_DEFAULT)
pattern LoadNoScale           = LoadFlags (#const FT_LOAD_NO_SCALE)
pattern LoadNoHinting         = LoadFlags (#const FT_LOAD_NO_HINTING)
pattern LoadRender            = LoadFlags (#const FT_LOAD_RENDER)
pattern LoadNoBitmap          = LoadFlags (#const FT_LOAD_NO_BITMAP)
pattern LoadVerticalLayout    = LoadFlags (#const FT_LOAD_VERTICAL_LAYOUT)
pattern LoadForceAutohint     = LoadFlags (#const FT_LOAD_FORCE_AUTOHINT)
pattern LoadCropBitmap        = LoadFlags (#const FT_LOAD_CROP_BITMAP)
pattern LoadPedantic          = LoadFlags (#const FT_LOAD_PEDANTIC)
pattern LoadIgnoreGlobalAdvanceWidth = LoadFlags (#const FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH)
pattern LoadNoRecurse         = LoadFlags (#const FT_LOAD_NO_RECURSE)
pattern LoadIgnoreTransform   = LoadFlags (#const FT_LOAD_IGNORE_TRANSFORM)
pattern LoadMonochrome        = LoadFlags (#const FT_LOAD_MONOCHROME)
pattern LoadLinearDesign      = LoadFlags (#const FT_LOAD_LINEAR_DESIGN)
pattern LoadNoAutohint        = LoadFlags (#const FT_LOAD_NO_AUTOHINT)
pattern LoadColor             = LoadFlags (#const FT_LOAD_COLOR)
pattern LoadComputeMetrics    = LoadFlags (#const FT_LOAD_COMPUTE_METRICS)
pattern LoadBitmapMetricsOnly = LoadFlags (#const FT_LOAD_BITMAP_METRICS_ONLY)
pattern LoadTargetNormal      = LoadFlags (#const FT_LOAD_TARGET_NORMAL)
pattern LoadTargetLight       = LoadFlags (#const FT_LOAD_TARGET_LIGHT)
pattern LoadTargetMono        = LoadFlags (#const FT_LOAD_TARGET_MONO)
pattern LoadTargetLcd         = LoadFlags (#const FT_LOAD_TARGET_LCD)
pattern LoadTargetLcdV        = LoadFlags (#const FT_LOAD_TARGET_LCD_V)

-- |FT_GLYPH_FORMAT_* flags
newtype GlyphFormat = GlyphFormat { unwrapGlyphFormat :: CInt } deriving (Show)
pattern GlyphFormatComposite  = GlyphFormat (#const FT_GLYPH_FORMAT_COMPOSITE)
pattern GlyphFormatBitmap     = GlyphFormat (#const FT_GLYPH_FORMAT_BITMAP)
pattern GlyphFormatOutline    = GlyphFormat (#const FT_GLYPH_FORMAT_OUTLINE)
pattern GlyphFormatPlotter    = GlyphFormat (#const FT_GLYPH_FORMAT_PLOTTER)

data GlyphRec
-- |Wrapper for FT_Glyph, holding a font glyph.
type Glyph = Ptr GlyphRec

foreign import ccall unsafe "FT_Load_Glyph"
    c_loadGlyph :: Face -> CUInt -> CInt -> IO ErrorCode

foreign import ccall unsafe "FT_Get_Glyph"
    c_getGlyph :: GlyphSlot -> Ptr Glyph -> IO ErrorCode

-- |Load a glyph from a font face.
loadGlyph :: Face -> Int -> [LoadFlags] -> IO (Maybe Glyph)
loadGlyph face index flags = do
    err <- c_loadGlyph face (fromIntegral index) (foldl' (.|.) 0 $ map unwrapLoadFlags flags)
    if isError err
    then return Nothing
    else do
        slot <- peek $ c_glyph face
        alloca $ \glyph -> do
            err' <- c_getGlyph slot glyph
            if isError err'
            then return Nothing
            else Just <$> peek glyph

foreign import ccall unsafe "FT_Done_Glyph"
    c_doneGlyph :: Face -> IO ()

-- |Discard a glyph.
doneGlyph :: Face -> IO ()
doneGlyph = c_doneGlyph
