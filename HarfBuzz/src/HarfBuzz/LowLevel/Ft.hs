{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module HarfBuzz.LowLevel.Ft where

import Foreign.C.Types
import Foreign.Ptr

import qualified FreeType.LowLevel.FaceType as Ft
import HarfBuzz.LowLevel.Hb

foreign import ccall "hb_ft_face_create" faceCreate :: Ft.Face -> FunPtr (Ptr a -> IO ()) -> IO (Ptr Face)
foreign import ccall "hb_ft_face_create_cached" faceCreateCached :: Ft.Face -> IO (Ptr Face)
foreign import ccall "hb_ft_face_create_referenced" faceCreateReferenced :: Ft.Face -> IO (Ptr Face)
foreign import ccall "hb_ft_font_create" fontCreate :: Ft.Face -> FunPtr (Ptr a -> IO ()) -> IO (Ptr Font)
foreign import ccall "hb_ft_font_create_referenced" fontCreateReferenced :: Ft.Face -> IO (Ptr Font)
foreign import ccall "hb_ft_font_get_face" fontGetFace :: Ptr Font -> IO Ft.Face
foreign import ccall "hb_ft_font_set_load_flags" fontSetLoadFlags :: Ptr Font -> CInt -> IO ()
foreign import ccall "hb_ft_font_get_load_flags" fontGetLoadFlags :: Ptr Font -> IO CInt
foreign import ccall "hb_ft_font_changed" fontChanged :: Ptr Font -> IO ()
foreign import ccall "hb_ft_font_set_funcs" fontSetFuncs :: Ptr Font -> IO ()
