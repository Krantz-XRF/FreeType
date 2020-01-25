{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module FreeType.Rasterific.TextRender
    ( PenStatus
    , makePenStatus
    , TextRenderT
    , renderTextM
    , TextRender
    , renderText
    , printChar
    , printString
    ) where

import Data.String
import Data.List

import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.RWS.Class
import qualified Control.Monad.Trans.RWS.CPS as W

import Graphics.Rasterific as R

import FreeType.LowLevel.Types
import FreeType.Error
import FreeType.Face
import FreeType.Glyph
import FreeType.Kerning
import FreeType.LowLevel.GlyphMetrics
import FreeType.Outline hiding (c_outline)
import FreeType.LowLevel.GlyphSlot

import FreeType.Rasterific.Basic

import Foreign.Ptr
import Foreign.Storable

data PenStatus = PenStatus
    { fontSize :: !Float
    , penPosition :: !Point
    , lastFace :: !Face
    , lastGlyph :: !Int
    } deriving stock (Show, Eq)

makePenStatus :: Float -> Point -> PenStatus
makePenStatus sz pos = PenStatus
    { fontSize = sz
    , penPosition = pos
    , lastFace = nullPtr
    , lastGlyph = 0
    }

newtype TextRenderT m a
    = TextRenderT { unwrapTextRenderT :: W.RWST [Face] [CurveSegment] PenStatus m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

type TextRender = TextRenderT Identity

instance Monad m => MonadReader [Face] (TextRenderT m) where
    ask = TextRenderT W.ask
    local f = TextRenderT . W.local f . unwrapTextRenderT
    reader = TextRenderT . W.reader

instance Monad m => MonadState PenStatus (TextRenderT m) where
    state = TextRenderT . W.state
    get = TextRenderT W.get
    put = TextRenderT . W.put

instance Monad m => MonadWriter [CurveSegment] (TextRenderT m) where
    writer = TextRenderT . W.writer
    tell = TextRenderT . W.tell
    listen = TextRenderT . W.listen . unwrapTextRenderT
    pass = TextRenderT . W.pass . unwrapTextRenderT

instance Monad m => MonadRWS [Face] [CurveSegment] PenStatus (TextRenderT m)

instance MonadIO m => MonadIO (TextRenderT m) where
    liftIO = TextRenderT . liftIO

instance MonadIO m => MonadFail (TextRenderT m) where
    fail = liftIO . customError

renderTextM :: Monad m => [Face] -> PenStatus -> TextRenderT m a -> m [CurveSegment]
renderTextM f st (TextRenderT m) = snd <$> W.evalRWST m f st

renderText :: [Face] -> PenStatus -> TextRender a -> [CurveSegment]
renderText f st = runIdentity . renderTextM f st

firstM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstM [] _ = return Nothing
firstM (x:xs) f = f x >>= \case
    Nothing -> firstM xs f
    res -> return res

printChar :: MonadIO m => Char -> TextRenderT m (BBox Float)
printChar c = do
    -- Get char index, fail if not presented
    faces <- ask
    Just (face, idx) <- firstM faces $ \fc -> do
        i <- liftIO $ uncheckedGetCharIndex fc c
        return $ if i /= 0 then Just (fc, i) else Nothing
    -- Kerning support
    prevFace <- gets lastFace
    prevIdx <- gets lastGlyph
    kerningSupport <- liftIO $ hasKerning face
    when (kerningSupport && prevFace == face) $ do
        Vector dx dy <- liftIO $ getKerning face prevIdx idx KerningDefault
        modify $ \st -> st{ penPosition =
            let V2 x y = penPosition st
            in V2 (x + dx) (y + dy) }
    -- Record the font face and glyph index
    modify $ \st -> st{ lastFace = face, lastGlyph = idx }
    -- Load glyph outline
    liftIO $ loadGlyph face idx [LoadNoBitmap]
    slot <- liftIO $ peek $ c_glyph face
    let po = c_outline slot
    liftIO $ assert (po /= nullPtr) "Not an outline glyph."
    -- Current pen position
    V2 px py <- gets penPosition
    -- Print current outline
    liftIO $ outlineTransform po (Matrix @Float 1 0 0 (-1))
    liftIO $ outlineTranslate po px py
    outline <- liftIO $ extractBezier po 0 0
    tell outline
    -- Step pen position
    rawMetrics <- liftIO $ peek $ c_metrics @F26'6 slot
    let metrics = fromFixedPoint <$> rawMetrics
    hori <- liftIO $ hasHorizontal face
    vert <- liftIO $ hasVertical face
    let dx = if hori then horiAdvance metrics else 0
    let dy = if vert then vertAdvance metrics else 0
    modify $ \st -> st{ penPosition =
        let V2 x y = penPosition st
        in V2 (x + dx) (y + dy) }
    -- Calculate bounding box
    liftIO $ outlineGetBBox po

printString :: MonadIO m => String -> TextRenderT m (BBox Float)
printString str = foldl1' (<>) <$> mapM printChar str

instance (a ~ BBox Float, MonadIO m) => IsString (TextRenderT m a) where
    fromString = printString
