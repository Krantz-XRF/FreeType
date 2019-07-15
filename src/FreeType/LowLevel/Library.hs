{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FreeType.LowLevel.Library
    ( LibraryRec, Library
    , initFreeType, doneFreeType
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)

import FreeType.Error (ErrorCode(..), unwrapError)

data LibraryRec
-- |Wrapper for FT_Library, holding the FreeType library.
type Library = Ptr LibraryRec

foreign import ccall unsafe "FT_Init_FreeType"
    c_initFreeType :: Ptr Library -> IO ErrorCode

foreign import ccall unsafe "FT_Done_FreeType"
    c_doneFreeType :: Library -> IO ErrorCode

-- |Initialize the FreeType Library.
initFreeType :: IO Library
initFreeType = alloca $ \lib -> do
    unwrapError "Initialization failed." $ c_initFreeType lib
    peek lib

-- |Finalize the FreeType Library.
doneFreeType :: Library -> IO ()
doneFreeType lib = unwrapError "Finalization failed." $ c_doneFreeType lib
