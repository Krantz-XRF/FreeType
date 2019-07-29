{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
module FreeType.Outline
    ( module FreeType.LowLevel.Outline
    , newOutlineSVGPrinter
    , printOutlineSVG
    , Bezier(..)
    , extractBezier
    , BezierSegment(..)
    , AppendList, nil, append, unwrapAppendList
    ) where

import FreeType.LowLevel.Outline
import FreeType.LowLevel.Types

import System.IO (Handle, hPutStr)
import Text.Printf (printf, hPrintf)

import Data.IORef (newIORef, modifyIORef', readIORef)

-- |File header for exporting SVG images.
svgFileHeader :: BBox Double -> String
svgFileHeader BBox{..} = printf
    "<svg xmlns='http://www.w3.org/2000/svg'\n\
    \     xmlns:xlink='http://www.w3.org/1999/xlink'\n\
    \     viewBox='%f %f %f %f'>\n\
    \  <path d='\n"
    xMin yMin (xMax - xMin) (yMax - yMin)

-- |File footer for exporting SVG images.
svgFileFooter :: String -> String
svgFileFooter = printf
    "          '\n\
    \        fill='%s'/>\n\
    \</svg>"

-- |Printer for exporting SVG images.
newOutlineSVGPrinter :: Handle -> OutlineFuncs
newOutlineSVGPrinter h = OutlineFuncs
    { moveToFunc = \(Vector x y) -> hPrintf h "           M %f %f\n" x y
    , lineToFunc = \(Vector x y) -> hPrintf h "           L %f %f\n" x y
    , conicToFunc = \(Vector cx cy) (Vector x y) ->
        hPrintf h "           Q %f %f %f %f\n" cx cy x y
    , cubicToFunc = \(Vector c1x c1y) (Vector c2x c2y) (Vector x y) ->
        hPrintf h "           C %f %f %f %f %f %f\n" c1x c1y c2x c2y x y
    , shift = 0
    , delta = 0
    }

-- |Export SVG images.
printOutlineSVG :: String -> Handle -> POutline -> IO ()
printOutlineSVG color h po = do
    bbox <- outlineGetBBox po
    hPutStr h (svgFileHeader bbox)
    outlineDecompose po (newOutlineSVGPrinter h)
    hPutStr h (svgFileFooter color)

-- |Bezier curves.
class Bezier b where
    type ResultBezier b = (res :: *) | res -> b
    emptyBezier :: b
    resultBezier :: b -> ResultBezier b
    moveTo :: Vector Double -> b -> b
    lineTo :: Vector Double -> b -> b
    conicTo :: Vector Double -> Vector Double -> b -> b
    cubicTo :: Vector Double -> Vector Double -> Vector Double -> b -> b

    type ResultBezier b = b
    default resultBezier :: (ResultBezier b ~ b) => b -> ResultBezier b
    resultBezier = id
    {-# MINIMAL emptyBezier, moveTo, lineTo, conicTo, cubicTo #-}

-- |Extract a Bezier curve from an outline.
extractBezier :: Bezier b => POutline -> Int -> Double -> IO (ResultBezier b)
extractBezier po sft dlt = do
    res <- newIORef emptyBezier
    outlineDecompose po OutlineFuncs
        { moveToFunc = modifyIORef' res . moveTo
        , lineToFunc = modifyIORef' res . lineTo
        , conicToFunc = \c v -> modifyIORef' res (conicTo c v)
        , cubicToFunc = \c1 c2 v -> modifyIORef' res (cubicTo c1 c2 v)
        , shift = sft
        , delta = dlt
        }
    resultBezier <$> readIORef res

-- |Bezier curve segments.
class BezierSegment b where
    lineFromTo :: Vector Double -> Vector Double -> b
    conicFromTo :: Vector Double -> Vector Double -> Vector Double -> b
    cubicFromTo :: Vector Double -> Vector Double -> Vector Double -> Vector Double -> b

-- |A list, appending is O(1) instead of prepending.
newtype AppendList a = AppendList [a]

-- |Empty AppendList.
nil :: AppendList a
nil = AppendList []

-- |Append to an AppendList.
append :: AppendList a -> a -> AppendList a
append (AppendList xs) x = AppendList (x:xs)

-- |Get a list from an AppendList.
unwrapAppendList :: AppendList a -> [a]
unwrapAppendList (AppendList xs) = reverse xs

-- |Build a Bezier curve out of some BezierSegment b.
type BezierBuilder b = (Vector Double, AppendList b)

instance BezierSegment b => Bezier (BezierBuilder b) where
    type ResultBezier (BezierBuilder b) = [b]
    emptyBezier = (Vector 0 0, nil)
    resultBezier = unwrapAppendList . snd
    moveTo  v       (_,  lst) = (v, lst)
    lineTo  v       (lp, lst) = (v, append lst $ lineFromTo lp v)
    conicTo c  v    (lp, lst) = (v, append lst $ conicFromTo lp c v)
    cubicTo c1 c2 v (lp, lst) = (v, append lst $ cubicFromTo lp c1 c2 v)
