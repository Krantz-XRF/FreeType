{-# LANGUAGE EmptyDataDecls #-}
module FreeType.LowLevel.Size where

import Foreign.Ptr

data SizeRec
-- |Wrapper for FT_Size
type Size = Ptr SizeRec
