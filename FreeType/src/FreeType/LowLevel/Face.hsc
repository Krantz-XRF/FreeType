{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module FreeType.LowLevel.Face
    ( FaceRec, Face
    , newFace, doneFace
    , c_numFaces
    , c_faceIndex
    , c_faceFlags
    , c_styleFlags
    , c_numGlyphs
    , c_familyName
    , c_styleName
    , c_numFixedSizes
    , c_numCharmaps
    , c_generic
    , c_bbox
    , c_unitsPerEM
    , c_ascender
    , c_descender
    , c_height
    , c_maxAdvanceWidth
    , c_maxAdvanceHeight
    , c_underlinePosition
    , c_underlineThickness
    , c_glyph
    , c_size
    , getCharIndex
    , uncheckedGetCharIndex
    , setCharSize
    , setPixelSizes
    , FaceFlag
        ( ..
        , Scalable
        , FixedSizes
        , FixedWidth
        , Sfnt
        , Horizontal
        , Vertical
        , Kerning
        , FastGlyphs
        , MultipleMasters
        , GlyphNames
        , ExternalStream
        , Hinter
        , CidKeyed
        , Tricky
        , Color
        , Variation
        )
    , hasFaceFlag
    , hasHorizontal
    , hasVertical
    , hasKerning
    ) where

import Data.Bits

import Foreign.C.Types
import Foreign.C.String (CString, withCString)
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)

import FreeType.LowLevel.FaceType (FaceRec, Face)
import FreeType.LowLevel.Types (toFixedPoint, F26'6(..), BBox(..))
import FreeType.LowLevel.Generic (Generic)
import FreeType.LowLevel.Size (Size)
import FreeType.LowLevel.Library (Library)
import FreeType.LowLevel.GlyphSlot (GlyphSlot)
import FreeType.Error (ErrorCode(..), unwrapError, assert)

#include <ft2build.h>
#include FT_FREETYPE_H

foreign import ccall unsafe "FT_New_Face"
    c_newFace :: Library -> CString -> CInt -> Ptr Face -> IO ErrorCode

foreign import ccall unsafe "FT_Done_Face"
    c_doneFace :: Face -> IO ErrorCode

-- |Load a font face from file.
newFace :: Library -> String -> Int -> IO Face
newFace lib path cnt =
    withCString path $ \cpath ->
    alloca $ \face -> do
        unwrapError "Failed to load a font face." $
            c_newFace lib cpath (fromIntegral cnt) face
        peek face

-- |Discard a font face.
doneFace :: Face -> IO ()
doneFace = unwrapError "Failed to discard a font face." . c_doneFace

c_numFaces :: Face -> Ptr CLong
c_numFaces = #ptr FT_FaceRec, num_faces

c_faceIndex :: Face -> Ptr CLong
c_faceIndex = #ptr FT_FaceRec, face_index

c_faceFlags :: Face -> Ptr FaceFlag
c_faceFlags = #ptr FT_FaceRec, face_flags

c_styleFlags :: Face -> Ptr CLong
c_styleFlags = #ptr FT_FaceRec, style_flags

c_numGlyphs :: Face -> Ptr CLong
c_numGlyphs = #ptr FT_FaceRec, num_glyphs

c_familyName :: Face -> Ptr CString
c_familyName = #ptr FT_FaceRec, family_name

c_styleName :: Face -> Ptr CString
c_styleName = #ptr FT_FaceRec, style_name

c_numFixedSizes :: Face -> Ptr CInt
c_numFixedSizes = #ptr FT_FaceRec, num_fixed_sizes

-- available_sizes :: Face -> Ptr (Ptr BS.FT_Bitmap_Size)
-- available_sizes = #ptr FT_FaceRec, available_sizes

c_numCharmaps :: Face -> Ptr CInt
c_numCharmaps = #ptr FT_FaceRec, num_charmaps

-- charmaps :: Face -> Ptr (Ptr CM.FT_CharMap)
-- charmaps = #ptr FT_FaceRec, charmaps

c_generic :: Face -> Ptr (Generic a)
c_generic = #ptr FT_FaceRec, generic

c_bbox :: Face -> Ptr (BBox a)
c_bbox = #ptr FT_FaceRec, bbox

c_unitsPerEM :: Face -> Ptr CUShort
c_unitsPerEM = #ptr FT_FaceRec, units_per_EM

c_ascender :: Face -> Ptr CShort
c_ascender = #ptr FT_FaceRec, ascender

c_descender :: Face -> Ptr CShort
c_descender = #ptr FT_FaceRec, descender

c_height :: Face -> Ptr CShort
c_height = #ptr FT_FaceRec, height

c_maxAdvanceWidth :: Face -> Ptr CShort
c_maxAdvanceWidth = #ptr FT_FaceRec, max_advance_width

c_maxAdvanceHeight :: Face -> Ptr CShort
c_maxAdvanceHeight = #ptr FT_FaceRec, max_advance_height

c_underlinePosition :: Face -> Ptr CShort
c_underlinePosition = #ptr FT_FaceRec, underline_position

c_underlineThickness :: Face -> Ptr CShort
c_underlineThickness = #ptr FT_FaceRec, underline_thickness

-- |Field glyph for a font face.
c_glyph :: Face -> Ptr GlyphSlot
c_glyph = #ptr FT_FaceRec, glyph

c_size :: Face -> Ptr Size
c_size = #ptr FT_FaceRec, size

-- charmap :: Face -> Ptr CM.FT_CharMap
-- charmap = #ptr FT_FaceRec, charmap

foreign import ccall unsafe "FT_Get_Char_Index"
    c_getCharIndex :: Face -> CULong -> IO CUInt

-- |Get a character index from face.
-- If the character is not presented, an exception is raised.
getCharIndex :: Face -> Char -> IO Int
getCharIndex face c = do
    idx <- c_getCharIndex face (fromIntegral $ fromEnum c)
    assert (idx /= 0) "Failed to get character index from font face."
    return $ fromIntegral idx

-- |Get a character index from face.
-- If the character is not presented, 0 is returned.
uncheckedGetCharIndex :: Face -> Char -> IO Int
uncheckedGetCharIndex face = fmap fromIntegral . c_getCharIndex face . fromIntegral . fromEnum

foreign import ccall unsafe "FT_Set_Char_Size"
    c_setCharSize :: Face -> F26'6 -> F26'6 -> CUInt -> CUInt -> IO ErrorCode

-- |Set character size (in points) for a font face.
setCharSize :: RealFrac a => Face -> a -> a -> Int -> Int -> IO ()
setCharSize face w h wres hres
    = unwrapError "Failed to set pixel size for font face."
    $ c_setCharSize face w' h' (fromIntegral wres) (fromIntegral hres)
    where w' = toFixedPoint w
          h' = toFixedPoint h

foreign import ccall unsafe "FT_Set_Pixel_Sizes"
    c_setPixelSizes :: Face -> CUInt -> CUInt -> IO ErrorCode

-- |Set pixel size for a font face.
setPixelSizes :: Face -> Int -> Int -> IO ()
setPixelSizes face w h
    = unwrapError "Failed to set pixel size for font face."
    $ c_setPixelSizes face (fromIntegral w) (fromIntegral h)

newtype FaceFlag = FaceFlag { getFaceFlag :: CLong }
    deriving newtype (Eq, Bits, Storable)

instance Semigroup FaceFlag where
    (<>) = (.|.)

instance Monoid FaceFlag where
    mempty = FaceFlag 0

pattern Scalable        = FaceFlag (#const FT_FACE_FLAG_SCALABLE)
pattern FixedSizes      = FaceFlag (#const FT_FACE_FLAG_FIXED_SIZES)
pattern FixedWidth      = FaceFlag (#const FT_FACE_FLAG_FIXED_WIDTH)
pattern Sfnt            = FaceFlag (#const FT_FACE_FLAG_SFNT)
pattern Horizontal      = FaceFlag (#const FT_FACE_FLAG_HORIZONTAL)
pattern Vertical        = FaceFlag (#const FT_FACE_FLAG_VERTICAL)
pattern Kerning         = FaceFlag (#const FT_FACE_FLAG_KERNING)
pattern FastGlyphs      = FaceFlag (#const FT_FACE_FLAG_FAST_GLYPHS)
pattern MultipleMasters = FaceFlag (#const FT_FACE_FLAG_MULTIPLE_MASTERS)
pattern GlyphNames      = FaceFlag (#const FT_FACE_FLAG_GLYPH_NAMES)
pattern ExternalStream  = FaceFlag (#const FT_FACE_FLAG_EXTERNAL_STREAM)
pattern Hinter          = FaceFlag (#const FT_FACE_FLAG_HINTER)
pattern CidKeyed        = FaceFlag (#const FT_FACE_FLAG_CID_KEYED)
pattern Tricky          = FaceFlag (#const FT_FACE_FLAG_TRICKY)
pattern Color           = FaceFlag (#const FT_FACE_FLAG_COLOR)
pattern Variation       = FaceFlag (#const FT_FACE_FLAG_VARIATION)

hasFaceFlag :: Face -> FaceFlag -> IO Bool
hasFaceFlag face flag = do
    f <- peek (c_faceFlags face)
    return $ f .&. flag /= mempty

hasHorizontal, hasVertical, hasKerning :: Face -> IO Bool
hasHorizontal = flip hasFaceFlag Horizontal
hasVertical   = flip hasFaceFlag Vertical
hasKerning    = flip hasFaceFlag Kerning
