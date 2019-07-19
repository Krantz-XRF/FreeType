{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (mapM_, when)
import System.Exit (exitWith, ExitCode(..))

import Text.Read (readMaybe)
import Text.Printf (printf)

import Options
import FreeType.Library
import FreeType.Face
import FreeType.Glyph

defaultDebugLevel :: Int
defaultDebugLevel = 2

data Options = Options
    { printVersion :: Bool
    , helpMessage :: Bool
    , debugOutput :: Int
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { printVersion = False
    , helpMessage = False
    , debugOutput = 0
    }

options :: [OptDescr String Options]
options =
    [ Option ['V'] ["version"] "Print version number"
        $ NoArg $ \opt -> return opt{ printVersion = True }
    , Option ['h'] ["help"] "Print this help message"
        $ NoArg $ \opt -> return opt{ helpMessage = True }
    , Option [] ["debug"] "Set debug level."
        $ OptArg "debug level"
        $ \str opt -> case str of
                Nothing -> return opt{ debugOutput = defaultDebugLevel }
                Just s -> case readMaybe s of
                    Nothing -> Left "--debug need a number as debug level"
                    Just lv -> return opt{ debugOutput = lv }
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
            when (printVersion || helpMessage) $ exitWith ExitSuccess
            mapM_ mainProc rest

mainProc :: String -> IO ()
mainProc path =
    withFreeType $ \lib ->
    withFace lib path 0 $ \face -> do
        setCharSize face 0 16.0 0 0
        putStrLn "FreeType: Ready."
        printf "Font face loaded: %s (%s)\n" path (show face)
        g <- loadCharGlyph face 'A' [LoadNoBitmap]
        case g of
            Nothing -> putStrLn "Failed to load glyph of 'A'."
            Just g' -> printf "Glyph loaded: A (%s)\n" (show g')