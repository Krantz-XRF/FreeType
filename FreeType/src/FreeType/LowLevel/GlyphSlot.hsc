{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FreeType.LowLevel.GlyphSlot
    ( GlyphSlotRec, GlyphSlot
    , c_face
    , c_next
    , c_generic
    , c_metrics
    , c_linearHoriAdvance
    , c_linearVertAdvance
    , c_advance
    , c_format
    , c_bitmapLeft
    , c_bitmapTop
    , c_outline
    , c_numSubglyphs
    , c_controlData
    , c_controlLen
    , c_lsbDelta
    , c_rsbDelta
    , getGlyph
    ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc

import FreeType.LowLevel.FaceType (Face)
import FreeType.LowLevel.Types (F16'16(..), F26'6(..), Vector(..))
import FreeType.LowLevel.Outline (Outline)
import FreeType.LowLevel.Glyph (Glyph, GlyphFormat)
import FreeType.LowLevel.GlyphMetrics (GlyphMetrics)
import FreeType.LowLevel.Generic (Generic)
import FreeType.Error (ErrorCode(..), unwrapError)

data GlyphSlotRec
-- |Wrapper for FT_GlyphSlot
type GlyphSlot = Ptr GlyphSlotRec

#include "ft2build.h"
#include FT_FREETYPE_H

c_face :: GlyphSlot -> Ptr Face
c_face = #ptr FT_GlyphSlotRec, face

c_next :: GlyphSlot -> Ptr GlyphSlot
c_next = #ptr FT_GlyphSlotRec, next

c_generic :: GlyphSlot -> Ptr (Generic a)
c_generic = #ptr FT_GlyphSlotRec, generic

c_metrics :: GlyphSlot -> Ptr (GlyphMetrics a)
c_metrics = #ptr FT_GlyphSlotRec, metrics

c_linearHoriAdvance :: GlyphSlot -> Ptr F16'16
c_linearHoriAdvance = #ptr FT_GlyphSlotRec, linearHoriAdvance

c_linearVertAdvance :: GlyphSlot -> Ptr F16'16
c_linearVertAdvance = #ptr FT_GlyphSlotRec, linearVertAdvance

c_advance :: GlyphSlot -> Ptr (Vector F26'6)
c_advance = #ptr FT_GlyphSlotRec, advance

c_format :: GlyphSlot -> Ptr GlyphFormat
c_format = #ptr FT_GlyphSlotRec, format

-- c_bitmap :: GlyphSlot -> Ptr FT_Bitmap
-- c_bitmap = #ptr FT_GlyphSlotRec, bitmap

c_bitmapLeft :: GlyphSlot -> Ptr CInt
c_bitmapLeft = #ptr FT_GlyphSlotRec, bitmap_left

c_bitmapTop :: GlyphSlot -> Ptr CInt
c_bitmapTop = #ptr FT_GlyphSlotRec, bitmap_top

c_outline :: GlyphSlot -> Ptr Outline
c_outline = #ptr FT_GlyphSlotRec, outline

c_numSubglyphs :: GlyphSlot -> Ptr CUInt
c_numSubglyphs = #ptr FT_GlyphSlotRec, num_subglyphs

-- c_subglyphs :: GlyphSlot -> Ptr FT_SubGlyph
-- c_subglyphs = #ptr FT_GlyphSlotRec, subglyphs

c_controlData :: GlyphSlot -> Ptr a
c_controlData = #ptr FT_GlyphSlotRec, control_data

c_controlLen :: GlyphSlot -> Ptr CLong
c_controlLen = #ptr FT_GlyphSlotRec, control_len

c_lsbDelta :: GlyphSlot -> Ptr F26'6
c_lsbDelta = #ptr FT_GlyphSlotRec, lsb_delta

c_rsbDelta :: GlyphSlot -> Ptr F26'6
c_rsbDelta = #ptr FT_GlyphSlotRec, rsb_delta

foreign import ccall unsafe "FT_Get_Glyph"
    c_getGlyph :: GlyphSlot -> Ptr Glyph -> IO ErrorCode

-- |Get a glyph from a glyph slot.
getGlyph :: GlyphSlot -> IO Glyph
getGlyph slot = alloca $ \glyph -> do
    unwrapError "Failed to get glyph from slot." $ c_getGlyph slot glyph
    peek glyph
