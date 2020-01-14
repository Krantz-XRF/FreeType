{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module FreeType.Rasterific
    ( TextOutline
    , translate
    , renderChar
    , renderCharAt
    ) where

import Data.Coerce
import Data.Foldable

import Graphics.Rasterific as R

import FreeType.Face as F
import FreeType.Types as F
import FreeType.Outline as F
import FreeType.Utils as F

newtype CurveSegment = CurveSegment Primitive deriving (Show, Eq)

-- |Bezier curves rendered from texts.
data TextOutline = TextOutline
    { textOutlineCurves :: [CurveSegment]
    , textBoundingBox :: BBox Float
    } deriving (Show, Eq)

vecToV2 :: F.Vector a -> V2 a
vecToV2 (F.Vector x y) = V2 x y

instance BezierSegment CurveSegment where
    type BSCoord CurveSegment = Float
    lineFromTo p1 p2
        = CurveSegment $ LinePrim
        $ Line (vecToV2 p1) (vecToV2 p2)
    conicFromTo p1 p2 p3
        = CurveSegment $ BezierPrim
        $ Bezier (vecToV2 p1) (vecToV2 p2) (vecToV2 p3)
    cubicFromTo p1 p2 p3 p4
        = CurveSegment $ CubicBezierPrim
        $ CubicBezier (vecToV2 p1) (vecToV2 p2) (vecToV2 p3) (vecToV2 p4)

instance Primitivable CurveSegment where
    toPrim = coerce

instance Geometry CurveSegment where
    toPrimitives c = [toPrim c]
    listToPrims = coerce . toList

instance Geometry TextOutline where
    toPrimitives = coerce . textOutlineCurves

translatePrim :: (Coercible a Primitive, Coercible Primitive a) => Point -> a -> a
translatePrim pt prim = coerce $ case coerce prim of
    LinePrim (Line p1 p2) ->
        LinePrim $ Line (pt + p1) (pt + p2)
    BezierPrim (Bezier p1 p2 p3) ->
        BezierPrim $ Bezier (pt + p1) (pt + p2) (pt + p3)
    CubicBezierPrim (CubicBezier p1 p2 p3 p4) ->
        CubicBezierPrim $ CubicBezier (pt + p1) (pt + p2) (pt + p3) (pt + p4)

-- |Translate the outline so that the origin lands on the given point.
translate :: Point -> TextOutline -> TextOutline
translate pt t =
    let outline = textOutlineCurves t
    in t{ textOutlineCurves = map (translatePrim pt) outline }

-- |Render the character with its origin still.
renderChar :: Face -> Char -> IO TextOutline
renderChar fc ch = do
    po <- loadOutline fc ch
    outlineTransform po (Matrix @Float 1 0 0 (-1))
    outline <- extractBezier po 0 0
    bbox <- outlineGetBBox po
    return $ TextOutline outline bbox

-- |Render the character and translate the origin.
-- See 'renderCharAt' and 'translate'.
renderCharAt :: Face -> Char -> Point -> IO TextOutline
renderCharAt fc ch p = translate p <$> renderChar fc ch
