{-# LANGUAGE EmptyDataDecls #-}
module FreeType.LowLevel.FaceType
    ( FaceRec, Face
    ) where

import Foreign.Ptr (Ptr)

data FaceRec
-- |Wrapper for Face, holding a font face.
type Face = Ptr FaceRec
