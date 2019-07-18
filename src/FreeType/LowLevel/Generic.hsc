{-# LANGUAGE ForeignFunctionInterface #-}
module FreeType.LowLevel.Generic
    ( Generic(genericData), finalizer
    , withGeneric
    , peekGeneric
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Wrapper for FT_Generic
data Generic a = Generic
    { genericData :: Ptr a
    -- ^Get the data pointer from Generic
    , finalizerRaw :: FunPtr (Ptr a -> IO ())
    }

-- |Get the finalizer for a Generic
finalizer :: Generic a -> Ptr a -> IO ()
finalizer = unwrapFinalizer . finalizerRaw

instance Storable (Generic a) where
    sizeOf _    = #size FT_Generic
    alignment _ = #alignment FT_Generic
    peek p = do
        d <- (#peek FT_Generic, data) p
        f <- (#peek FT_Generic, finalizer) p
        return $ Generic d f
    poke p (Generic d f) = do
        (#poke FT_Generic, data) p d
        (#poke FT_Generic, finalizer) p f

foreign import ccall "wrapper"
    wrapFinalizer :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall "dynamic"
    unwrapFinalizer :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

-- |Create a temporary Generic object.
withGeneric :: Ptr a -> (Ptr a -> IO ()) -> (Generic a -> IO b) -> IO b
withGeneric d f proc = do
    wf <- wrapFinalizer f
    res <- proc (Generic d wf)
    freeHaskellFunPtr wf
    return res

-- |Peek an existing Generic object.
peekGeneric :: Ptr (Generic a) -> (Ptr a -> (Ptr a -> IO ()) -> IO b) -> IO b
peekGeneric p proc = do
    Generic d wf <- peek p
    proc d (unwrapFinalizer wf)
