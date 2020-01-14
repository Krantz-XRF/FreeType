{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Maybe (listToMaybe)

import Control.Monad (mapM_, when)
import System.Exit (exitSuccess)
import System.IO (withFile, IOMode(..), FilePath, Handle, stdout)

import Text.Read (readMaybe)
import Text.Printf (printf)

import Options
import FreeType.Library
import FreeType.Face
import FreeType.Outline
import FreeType.Types
import FreeType.Utils
import FreeType.Rasterific

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture

defaultDebugLevel :: Int
defaultDebugLevel = 2

data Options = Options
    { printVersion :: Bool
    , helpMessage :: Bool
    , debugOutput :: Int
    , outputFile :: Maybe String
    , extractChar :: Maybe Char
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { printVersion = False
    , helpMessage = False
    , debugOutput = 0
    , outputFile = Nothing
    , extractChar = Nothing
    }

options :: [OptDescr String Options]
options =
    [ Option ['V'] ["version"] "Print version number."
        $ NoArg $ \opt -> return opt{ printVersion = True }
    , Option ['h'] ["help"] "Print this help message."
        $ NoArg $ \opt -> return opt{ helpMessage = True }
    , Option [] ["debug"] "Set debug level."
        $ OptArg "debug level"
        $ \str opt -> case str of
                Nothing -> return opt{ debugOutput = defaultDebugLevel }
                Just s -> case readMaybe s of
                    Nothing -> Left "--debug need a number as debug level"
                    Just lv -> return opt{ debugOutput = lv }
    , Option ['o'] ["output"] "Set output file."
        $ ReqArg "output file"
        $ \str opt -> return opt{ outputFile = Just str }
    , Option ['c'] ["extract", "char"] "Select the character to extract."
        $ ReqArg "char"
        $ \str opt -> return opt{ extractChar = listToMaybe str }
    ]

main :: IO ()
main = do
    (eopts, rest, errs) <- getOptions options defaultOptions
    if not $ null errs
    then mapM_ putStrLn errs
    else case eopts of
        Left err -> putStrLn err
        Right Options{..} -> do
            when (null rest || printVersion) $ putStrLn "FreeType-convert 2019"
            when (null rest || helpMessage) $ putStrLn $ usageInfo "Usage:" options
            when (printVersion || helpMessage) exitSuccess
            case rest of
                [] -> putStrLn "No input file."
                (f:_) -> case extractChar of
                    Nothing -> putStrLn "No character to extract."
                    Just c -> mainProc f c outputFile

withFileOr :: Maybe FilePath -> IOMode -> Handle -> (Handle -> IO a) -> IO a
withFileOr Nothing  _ h proc = proc h
withFileOr (Just f) m _ proc = withFile f m proc

mainProc :: String -> Char -> Maybe FilePath -> IO ()
mainProc path ch outPath =
    withFreeType $ \lib ->
    withFace lib path 0 $ \face -> do
        setCharSize @Double face 0 32 0 0
        putStrLn "FreeType: Ready."
        printf "Font face loaded: %s (%s)\n" path (show face)
        withFileOr (fmap (++ ".svg") outPath) WriteMode stdout $ \file -> do
            (po, metrics) <- loadOutlineAndMetrics @Double face ch
            outlineTransform po (Matrix @Double 1 0 0 (-1))
            printOutlineSVG "black" file po
            print metrics
        case outPath of
            Nothing -> return ()
            Just name -> do
                outline <- renderCharAt face ch (V2 40 40)
                let white = PixelRGBA8 255 255 255 255
                let transparent = PixelRGBA8 0 0 0 0
                let img = renderDrawing 80 60 transparent
                        $ withTexture (uniformTexture white)
                        $ fill outline
                writePng (name ++ ".png") img
