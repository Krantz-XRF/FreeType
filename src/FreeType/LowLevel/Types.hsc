{-# LANGUAGE TypeFamilies #-}
module FreeType.LowLevel.Types
    ( module Data.FixedPoint
    , F26'6(..)
    , F16'16(..)
    , Vector(..)
    , BBox(..)
    ) where

import Data.FixedPoint

import Foreign.C.Types
import Foreign.Storable

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Fixed point (26.6) number format:
-- 26 bits for integral, and 6 for fractional.
newtype F26'6 = F26'6 { unwrapF26'6 :: CLong } deriving (Eq, Ord)

instance FixedPoint F26'6 where
    type RawType F26'6 = CLong
    precision = Precision 6
    unwrapFixedPoint = unwrapF26'6
    wrapFixedPoint = F26'6

-- |Fixed point (2.14) number format.
newtype F2'14 = F2'14 { unwrapF2'14 :: CShort } deriving (Eq, Ord)

instance FixedPoint F2'14 where
    type RawType F2'14 = CShort
    precision = Precision 6
    unwrapFixedPoint = unwrapF2'14
    wrapFixedPoint = F2'14

-- |Fixed point (16.16) number format.
newtype F16'16 = F16'16 { unwrapF16'16 :: CLong } deriving (Eq, Ord)

instance FixedPoint F16'16 where
    type RawType F16'16 = CLong
    precision = Precision 16
    unwrapFixedPoint = unwrapF16'16
    wrapFixedPoint = F16'16

-- |Wrapper for FT_Vector.
data Vector a = Vector
    { x :: a    -- The horizontal coordinate.
    , y :: a    -- The vertical coordinate.
    } deriving (Show, Eq)

instance Storable a => Storable (Vector a) where
    sizeOf _    = #size FT_Vector
    alignment _ = #alignment FT_Vector
    peek p = do
        x <- (#peek FT_Vector, x) p
        y <- (#peek FT_Vector, y) p
        return (Vector x y)
    poke p (Vector x y) = do
        (#poke FT_Vector, x) p x
        (#poke FT_Vector, y) p y

-- |Wrapper for FT_BBox (the bounding box).
data BBox a = BBox
    { xMin :: a -- The horizontal minimum (left-most).
    , yMin :: a -- The vertical minimum (bottom-most).
    , xMax :: a -- The horizontal maximum (right-most).
    , yMax :: a -- The vertical maximum (top-most).
    } deriving (Show, Eq)

instance Storable a => Storable (BBox a) where
    sizeOf _    = #size FT_BBox
    alignment _ = #alignment FT_BBox
    peek p = do
        xMin <- (#peek FT_BBox, xMin) p
        yMin <- (#peek FT_BBox, yMin) p
        xMax <- (#peek FT_BBox, xMax) p
        yMax <- (#peek FT_BBox, yMax) p
        return (BBox xMin yMin xMax yMax)
    poke p (BBox xMin yMin xMax yMax) = do
        (#poke FT_BBox, xMin) p xMin
        (#poke FT_BBox, yMin) p yMin
        (#poke FT_BBox, xMax) p xMax
        (#poke FT_BBox, yMax) p yMax
