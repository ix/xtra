{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module Main where

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ReaderT, ask, local, runReaderT)
import Data.Bifunctor         (first, second)
import Data.Bits              ((.|.))
import Data.Int               (Int32)
import Numeric                (readHex)
import Options.Applicative
    (Parser, execParser, help, helper, info, long, maybeReader, option, short,
    strOption, value, (<**>))
import Safe                   (headMay)
import System.Environment     (lookupEnv)

import qualified Graphics.X11 as X

defaultFG :: X.Pixel
defaultFG = 0xFF_11_2B_3A

defaultBG :: X.Pixel
defaultBG = 0xFF_FF_ED_BB

data Arguments = Arguments
  { fontName   :: String
  , foreground :: X.Pixel
  , background :: X.Pixel
  }

data Environment = Environment
  { display      :: X.Display
  , root         :: X.Window
  , window       :: X.Window
  , content      :: [String]
  , font         :: X.FontStruct
  , scrollOffset :: (X.Position, X.Position)
  , theme        :: (X.Pixel, X.Pixel)
  }

data PositionedText = PositionedText
  { ptText  :: String
  , ptY     :: X.Position
  , ptWidth :: Int32
  }

main :: IO ()
main = do
  Arguments {..} <- execParser $ info (optionParser <**> helper) mempty
  display      <- maybe (error "Couldn't open display!") X.openDisplay =<< lookupEnv "DISPLAY"
  font         <- X.loadQueryFont display fontName
  content      <- lines <$> getContents
  root         <- X.rootWindow display 0
  window       <- X.createSimpleWindow display root 0 0 250 250 0 background background
  scrollOffset <- pure (0, 0)
  theme        <- pure (foreground, background)
  X.selectInput display window $ X.exposureMask .|. X.buttonPressMask
  X.mapWindow display window
  runReaderT eventLoop Environment {..}
  X.freeFont display font

readPixel :: String -> Maybe X.Pixel
readPixel ('#':xs) = readPixel  xs
readPixel xs       = readPixel' xs
  where readPixel' = fmap (.|. 0xFF_00_00_00) . fmap fst . headMay . readHex

optionParser :: Parser Arguments
optionParser = do
  fontName   <- strOption (long "font" <> short 'F' <> help "font name, as an x11 fontstring" <> value "fixed")
  foreground <- option (maybeReader readPixel) (long "foreground" <> short 'f' <> help "foreground color, hex" <> value defaultFG)
  background <- option (maybeReader readPixel) (long "background" <> short 'b' <> help "background color, hex" <> value defaultBG)
  pure Arguments {..}

eventLoop :: ReaderT Environment IO ()
eventLoop = do
  Environment {..} <- ask
  event <- liftIO $ X.allocaXEvent $
    \event -> X.nextEvent display event >> pure event

  liftIO $ do
    drawLines display window (fst theme) font scrollOffset content
    X.sync display False

  maybeButton <- liftIO $ X.get_EventType event >>= \type_ -> if
    | type_ == X.buttonPress -> Just <$> eventButton <$> X.get_ButtonEvent event
    | otherwise              -> pure Nothing

  case maybeButton of
    Just 4 -> local (\e -> e { scrollOffset = second (+ 10) scrollOffset }) eventLoop
    Just 5 -> local (\e -> e { scrollOffset = second (subtract 10) scrollOffset }) eventLoop
    Just 6 -> local (\e -> e { scrollOffset = first (+ 10) scrollOffset }) eventLoop
    Just 7 -> local (\e -> e { scrollOffset = first (subtract 10) scrollOffset }) eventLoop
    _      -> eventLoop

eventButton :: X.XButtonEvent -> X.Button
eventButton (_, _, _, _, _, _, _, _, button, _) = button

-- return the ascent and descent for a given font
fontMetrics :: X.FontStruct -> (Int32, Int32)
fontMetrics font = (ascent, descent)
  where (_, ascent, descent, _) = X.textExtents font ""

-- return character height for a given font
characterHeight :: X.FontStruct -> Int32
characterHeight = uncurry (+) . fontMetrics

-- arrange a list of strings vertically according to the font size
verticalize :: [String] -> X.FontStruct -> X.Position -> [PositionedText]
verticalize texts font origin = zipWith arrange [0..] texts
  where
    arrange index text = PositionedText text (origin + index * characterHeight font) $ X.textWidth font text

drawLines :: X.Display -> X.Window -> X.Pixel -> X.FontStruct -> (X.Position, X.Position) -> [String] -> IO ()
drawLines display window fgColor font origins texts = do
  gc   <- X.createGC display window
  let textPositions = verticalize texts font $ snd origins + characterHeight font
  X.clearWindow display window
  X.setForeground display gc fgColor
  X.setFont display gc $ X.fontFromFontStruct font
  forM_ textPositions $ \PositionedText {..} ->
    X.drawString display window gc (fst origins) ptY ptText
  X.freeGC display gc
