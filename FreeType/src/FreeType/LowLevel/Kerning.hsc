{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module FreeType.LowLevel.Kerning
    ( KerningMode
        ( ..
        , KerningDefault
        , KerningUnfitted
        , KerningUnscaled
        )
    , getKerning
    ) where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (alloca)

import FreeType.Error
import FreeType.LowLevel.FaceType
import FreeType.LowLevel.Types (fromFixedPoint, F26'6(..), Vector(..))

#include <ft2build.h>
#include FT_FREETYPE_H

newtype KerningMode = KerningMode { getKerningMode :: CUInt }
    deriving newtype (Eq, Storable)

pattern KerningDefault  = KerningMode (#const FT_KERNING_DEFAULT)
pattern KerningUnfitted = KerningMode (#const FT_KERNING_UNFITTED)
pattern KerningUnscaled = KerningMode (#const FT_KERNING_UNSCALED)

foreign import ccall unsafe "FT_Get_Kerning"
    c_getKerning :: Face -> CUInt -> CUInt -> KerningMode -> Ptr (Vector a) -> IO ErrorCode

rawGetKerning :: Storable a =>
    Face -> Int -> Int -> KerningMode -> (a -> b) -> (Vector b) -> IO (Vector b)
rawGetKerning fc prev cur mode conv defVal = alloca $ \vec -> do
    err <- c_getKerning fc (fromIntegral prev) (fromIntegral cur) mode vec
    if isOk err then fmap conv <$> peek vec else return defVal

getConv :: RealFrac a => KerningMode -> CLong -> a
getConv KerningUnscaled = fromIntegral
getConv _ = fromFixedPoint . F26'6

getKerning :: RealFrac a => Face -> Int -> Int -> KerningMode -> IO (Vector a)
getKerning fc prev cur mode = rawGetKerning fc prev cur mode (getConv mode) (Vector 0 0)
