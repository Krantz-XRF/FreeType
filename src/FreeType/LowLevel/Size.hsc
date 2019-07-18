{-# LANGUAGE EmptyDataDecls #-}
module FreeType.LowLevel.Size where

import Foreign.C.Types
import Foreign.Ptr

data SizeRec
-- |Wrapper for FT_Size
type Size = Ptr SizeRec
