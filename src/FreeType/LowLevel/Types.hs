{-# LANGUAGE TypeFamilies #-}
module FreeType.LowLevel.Types
    ( module Data.FixedPoint
    , F26'6(..)
    , F16'16(..)
    ) where

import Data.FixedPoint

import Foreign.C.Types

-- |Fixed point (26.6) number format:
-- 26 bits for integral, and 6 for fractional.
newtype F26'6 = F26'6 { unwrapF26'6 :: CLong } deriving (Eq, Ord)

instance FixedPoint F26'6 where
    type RawType F26'6 = CLong
    precision = Precision 6
    unwrapFixedPoint = unwrapF26'6
    wrapFixedPoint = F26'6

-- |Fixed point (16.16) number format.
newtype F16'16 = F16'16 { unwrapF16'16 :: CLong } deriving (Eq, Ord)

instance FixedPoint F16'16 where
    type RawType F16'16 = CLong
    precision = Precision 16
    unwrapFixedPoint = unwrapF16'16
    wrapFixedPoint = F16'16
