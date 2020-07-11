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
import Data.Ord               (comparing)
import Numeric                (readHex)
import Options.Applicative
    (Parser, execParser, help, helper, info, long, maybeReader, option, short,
    strOption, value, (<**>))
import Safe                   (headMay, maximumByDef)
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
  , gc           :: X.GC
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
  root         <- X.rootWindow display $ X.defaultScreen display
  window       <- X.createSimpleWindow display root 0 0 250 250 0 background background
  scrollOffset <- pure (0, 0)
  theme        <- pure (foreground, background)
  gc           <- X.createGC display window

  X.setForeground display gc $ fst theme
  X.setBackground display gc $ snd theme
  X.setFont display gc $ X.fontFromFontStruct font

  X.selectInput display window $ X.exposureMask .|. X.buttonPressMask
  X.mapWindow display window

  runReaderT (withPixmap eventLoop) Environment {..}

  X.freeFont display font
  X.freeGC display gc

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

eventLoop :: X.Pixmap -> ReaderT Environment IO ()
eventLoop source = do
  Environment {..} <- ask
  event <- liftIO $ X.allocaXEvent $
    \event -> X.nextEvent display event >> pure event

  liftIO $ do
    (_, _, _, width, height, _, _) <- X.getGeometry display window
    let (x, y) = scrollOffset
    X.clearWindow display window
    X.copyArea display source window gc x y width height 0 0
    X.sync display True

  maybeButton <- liftIO $ X.get_EventType event >>= \type_ -> if
    | type_ == X.buttonPress -> Just <$> eventButton <$> X.get_ButtonEvent event
    | otherwise              -> pure Nothing

  case maybeButton of
    Just 4 -> local (\e -> e { scrollOffset = second (+ 10) scrollOffset }) $ eventLoop source
    Just 5 -> local (\e -> e { scrollOffset = second (subtract 10) scrollOffset }) $ eventLoop source
    Just 6 -> local (\e -> e { scrollOffset = first (+ 10) scrollOffset }) $ eventLoop source
    Just 7 -> local (\e -> e { scrollOffset = first (subtract 10) scrollOffset }) $ eventLoop source
    _      -> eventLoop source

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
verticalize :: [String] -> X.FontStruct -> [PositionedText]
verticalize texts font = zipWith arrange [1..] texts
  where
    arrange index text = PositionedText text (index * characterHeight font) $ X.textWidth font text

withPixmap :: (X.Pixmap -> ReaderT Environment IO a) -> ReaderT Environment IO a
withPixmap actionM = do
  Environment {..} <- ask
  let height = fromIntegral $ characterHeight font * fromIntegral (length content)
  let width = fromIntegral $ X.textWidth font $ maximumByDef "" (comparing length) content
  pixmap <- liftIO $ X.createPixmap display window width height $ X.defaultDepth display $ X.defaultScreen display
  liftIO $ do
    X.setForeground display gc $ snd theme
    X.fillRectangle display pixmap gc 0 0 width height
    X.setForeground display gc $ fst theme
  renderContent pixmap
  result <- actionM pixmap
  liftIO $ X.freePixmap display pixmap
  pure result

renderContent :: X.Pixmap -> ReaderT Environment IO ()
renderContent target = do
  Environment {..} <- ask
  liftIO $ do
    let textPositions = verticalize content font
    forM_ textPositions $ \PositionedText {..} ->
      X.drawString display target gc (fst scrollOffset) ptY ptText
