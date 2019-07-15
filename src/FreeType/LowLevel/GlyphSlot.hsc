{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FreeType.LowLevel.GlyphSlot
    ( GlyphSlotRec, GlyphSlot
    ) where

import Foreign.Ptr

data GlyphSlotRec
-- |Wrapper for FT_GlyphSlot
type GlyphSlot = Ptr GlyphSlotRec
