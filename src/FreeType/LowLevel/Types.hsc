{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FreeType.LowLevel.Types
    ( module Data.FixedPoint
    , F26'6(..)
    , F16'16(..)
    , F2'14(..)
    , Vector(..)
    , BBox(..)
    , Matrix(..)
    ) where

import Data.FixedPoint

import Foreign.C.Types
import Foreign.Storable

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Fixed point (26.6) number format:
-- 26 bits for integral, and 6 for fractional.
newtype F26'6 = F26'6 { unwrapF26'6 :: CLong } deriving (Eq, Ord, Storable)

instance FixedPoint F26'6 where
    type RawType F26'6 = CLong
    precision = Precision 6
    unwrapFixedPoint = unwrapF26'6
    wrapFixedPoint = F26'6

-- |Fixed point (2.14) number format.
newtype F2'14 = F2'14 { unwrapF2'14 :: CShort } deriving (Eq, Ord, Storable)

instance FixedPoint F2'14 where
    type RawType F2'14 = CShort
    precision = Precision 6
    unwrapFixedPoint = unwrapF2'14
    wrapFixedPoint = F2'14

-- |Fixed point (16.16) number format.
newtype F16'16 = F16'16 { unwrapF16'16 :: CLong } deriving (Eq, Ord, Storable)

instance FixedPoint F16'16 where
    type RawType F16'16 = CLong
    precision = Precision 16
    unwrapFixedPoint = unwrapF16'16
    wrapFixedPoint = F16'16

-- |Wrapper for FT_Vector.
data Vector a = Vector
    { x :: a    -- ^The horizontal coordinate.
    , y :: a    -- ^The vertical coordinate.
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

instance Functor Vector where
    fmap f (Vector x y) = Vector (f x) (f y)

-- |Wrapper for FT_BBox (the bounding box).
data BBox a = BBox
    { xMin :: a -- ^The horizontal minimum (left-most).
    , yMin :: a -- ^The vertical minimum (bottom-most).
    , xMax :: a -- ^The horizontal maximum (right-most).
    , yMax :: a -- ^The vertical maximum (top-most).
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

instance Functor BBox where
    fmap f (BBox xMin yMin xMax yMax) = BBox (f xMin) (f yMin) (f xMax) (f yMax)

-- |Transformation matrix
data Matrix a = Matrix
    { xx :: a -- ^The (0,0) matrix coefficient.
    , xy :: a -- ^The (1,0) matrix coefficient.
    , yx :: a -- ^The (0,1) matrix coefficient.
    , yy :: a -- ^The (1,1) matrix coefficient.
    } deriving (Eq)

instance Storable a => Storable (Matrix a) where
    sizeOf _    = #size FT_Matrix
    alignment _ = #alignment FT_Matrix
    peek p = do
        xx <- (#peek FT_Matrix, xx) p
        xy <- (#peek FT_Matrix, xy) p
        yx <- (#peek FT_Matrix, yx) p
        yy <- (#peek FT_Matrix, yy) p
        return (Matrix xx xy yx yy)
    poke p (Matrix xx xy yx yy) = do
        (#poke FT_Matrix, xx) p xx
        (#poke FT_Matrix, xy) p xy
        (#poke FT_Matrix, yx) p yx
        (#poke FT_Matrix, yy) p yy

instance Functor Matrix where
    fmap f (Matrix xx xy yx yy) = Matrix (f xx) (f xy) (f yx) (f yy)
