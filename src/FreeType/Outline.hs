{-# LANGUAGE RecordWildCards #-}
module FreeType.Outline
    ( module FreeType.LowLevel.Outline
    , newOutlineSVGPrinter
    , printOutlineSVG
    ) where

import FreeType.LowLevel.Outline
import FreeType.LowLevel.Types

import System.IO (Handle, hPutStr)
import Text.Printf (printf, hPrintf)

-- |File header for exporting SVG images.
svgFileHeader :: BBox Int -> String
svgFileHeader BBox{..} = printf
    "<svg xmlns='http://www.w3.org/2000/svg'\n\
    \     xmlns:xlink='http://www.w3.org/1999/xlink'\n\
    \     viewBox='%d %d %d %d'>\n\
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
    { moveToFunc = \(Vector x y) -> hPrintf h "           M %d %d\n" x y
    , lineToFunc = \(Vector x y) -> hPrintf h "           L %d %d\n" x y
    , conicToFunc = \(Vector cx cy) (Vector x y) ->
        hPrintf h "           Q %d %d %d %d\n" cx cy x y
    , cubicToFunc = \(Vector c1x c1y) (Vector c2x c2y) (Vector x y) ->
        hPrintf h "           C %d %d %d %d %d %d\n" c1x c1y c2x c2y x y
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
