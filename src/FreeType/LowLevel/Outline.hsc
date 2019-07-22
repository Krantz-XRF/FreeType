{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
module FreeType.LowLevel.Outline where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import FreeType.LowLevel.Types
import FreeType.LowLevel.Library
import FreeType.Error

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Wrapper for FT_Outline.
data Outline

foreign import ccall unsafe "FT_Outline_Done"
    c_doneOutline :: Library -> Ptr Outline -> IO ErrorCode

-- |Discard an outline created by the library.
doneOutline :: Library -> Ptr Outline -> IO ()
doneOutline lib po = unwrapError "Failed to discard an outline." $ c_doneOutline lib po

-- |Wrapper for FT_Outline_Funcs.
data COutlineFuncs a = COutlineFuncs
    { c_moveToFunc :: FunPtr (Ptr (Vector CLong) -> Ptr a -> IO ())
    , c_lineToFunc :: FunPtr (Ptr (Vector CLong) -> Ptr a -> IO ())
    , c_conicToFunc :: FunPtr (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ())
    , c_cubicToFunc :: FunPtr (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ())
    , c_shift :: Int
    , c_delta :: Int
    }

instance Storable (COutlineFuncs a) where
    sizeOf _    = #size FT_Outline_Funcs
    alignment _ = #alignment FT_Outline_Funcs
    peek p = do
        c_moveToFunc <- (#peek FT_Outline_Funcs, move_to) p
        c_lineToFunc <- (#peek FT_Outline_Funcs, line_to) p
        c_conicToFunc <- (#peek FT_Outline_Funcs, conic_to) p
        c_cubicToFunc <- (#peek FT_Outline_Funcs, cubic_to) p
        c_shift <- (#peek FT_Outline_Funcs, shift) p
        c_delta <- (#peek FT_Outline_Funcs, delta) p
        return COutlineFuncs{..}
    poke p COutlineFuncs{..} = do
        (#poke FT_Outline_Funcs, move_to) p c_moveToFunc
        (#poke FT_Outline_Funcs, line_to) p c_lineToFunc
        (#poke FT_Outline_Funcs, conic_to) p c_conicToFunc
        (#poke FT_Outline_Funcs, cubic_to) p c_cubicToFunc
        (#poke FT_Outline_Funcs, shift) p c_shift
        (#poke FT_Outline_Funcs, delta) p c_delta

foreign import ccall "FT_Outline_Decompose"
    c_outlineDecompose :: Ptr Outline -> Ptr (COutlineFuncs a) -> Ptr a -> IO ErrorCode

foreign import ccall "wrapper"
    wrapOutlineFunc1
    :: (Ptr (Vector CLong) -> Ptr a -> IO ())
    -> IO (FunPtr (Ptr (Vector CLong) -> Ptr a -> IO ()))

foreign import ccall "wrapper"
    wrapOutlineFunc2
    :: (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ())
    -> IO (FunPtr (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ()))

foreign import ccall "wrapper"
    wrapOutlineFunc3
    :: (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ())
    -> IO (FunPtr (Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr (Vector CLong) -> Ptr a -> IO ()))

-- |Outline handler functions.
data OutlineFuncs = OutlineFuncs
    { moveToFunc :: (Vector Int) -> IO ()
    , lineToFunc :: (Vector Int) -> IO ()
    , conicToFunc :: (Vector Int) -> (Vector Int) -> IO ()
    , cubicToFunc :: (Vector Int) -> (Vector Int) -> (Vector Int) -> IO ()
    , shift :: Int
    , delta :: Int
    }

-- |Decompose an outline with the given handlers.
outlineDecompose :: Ptr Outline -> OutlineFuncs -> IO ()
outlineDecompose po OutlineFuncs{..} = alloca $ \funcs -> do
    let ipeek = fmap (fmap fromIntegral) . peek
    c_moveToFunc <- wrapOutlineFunc1 $ \v _ -> ipeek v >>= moveToFunc
    c_lineToFunc <- wrapOutlineFunc1 $ \v _ -> ipeek v >>= lineToFunc
    c_conicToFunc <- wrapOutlineFunc2 $ \c v _ -> do
        vc <- ipeek c
        vv <- ipeek v
        conicToFunc vc vv
    c_cubicToFunc <- wrapOutlineFunc3 $ \c1 c2 v _ -> do
        vc1 <- ipeek c1
        vc2 <- ipeek c2
        vv <- ipeek v
        cubicToFunc vc1 vc2 vv
    let c_shift = fromIntegral shift
    let c_delta = fromIntegral delta
    poke funcs COutlineFuncs{..}
    unwrapError "Failed to decompose outline." $ c_outlineDecompose po funcs nullPtr
    freeHaskellFunPtr c_moveToFunc
    freeHaskellFunPtr c_lineToFunc
    freeHaskellFunPtr c_conicToFunc
    freeHaskellFunPtr c_cubicToFunc
