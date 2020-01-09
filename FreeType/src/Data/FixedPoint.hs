{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.FixedPoint where

import Data.Bits (shiftL)

-- |Precision for a fixed point number, i.e. the length of fractional part.
newtype Precision a = Precision { unwrapPrecision :: Int }

-- |Fixed point numbers.
class Integral (RawType a) => FixedPoint a where
    type RawType a :: *
    precision :: Precision a
    unwrapFixedPoint :: a -> RawType a
    wrapFixedPoint :: RawType a -> a

-- |Convert from fixed point numbers to Fractional's.
fromFixedPoint :: forall a b . (FixedPoint a, Fractional b) => a -> b
fromFixedPoint x =
    let v = fromIntegral $ unwrapFixedPoint x
        p = unwrapPrecision $ precision @a
        base = fromIntegral $ (1 :: Int) `shiftL` p
    in v / base

-- |Convert back to fixed point numbers from RealFrac's.
toFixedPoint :: forall a b . (FixedPoint a, RealFrac b) => b -> a
toFixedPoint x =
    let p = unwrapPrecision $ precision @a
        base = fromIntegral $ (1 :: Int) `shiftL` p
    in wrapFixedPoint $ round $ x * base
