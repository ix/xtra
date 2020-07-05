{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module Main where

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ReaderT, ask, runReaderT)
import Data.Int               (Int32)
import Options.Applicative
    (Parser, execParser, helper, info, long, short, strOption, value, (<**>))
import System.Environment     (lookupEnv)

import qualified Graphics.X11 as X

backgroundColor :: X.Pixel
backgroundColor = 0xFF_FF_ED_BB

textColor :: X.Pixel
textColor = 0xFF_11_2B_3A

data Environment = Environment
  { display :: X.Display
  , root    :: X.Window
  , window  :: X.Window
  , content :: [String]
  , font    :: X.FontStruct
  }

data PositionedText = PositionedText
  { ptText  :: String
  , ptY     :: X.Position
  , ptWidth :: Int32
  }

main :: IO ()
main = do
  display  <- maybe (error "Couldn't open display!") X.openDisplay =<< lookupEnv "DISPLAY"
  font     <- X.loadQueryFont display =<< execParser (info (optionParser <**> helper) mempty)
  content  <- lines <$> getContents
  root     <- X.rootWindow display 0
  window   <- X.createSimpleWindow display root 0 0 250 250 0 backgroundColor backgroundColor
  X.selectInput display window X.exposureMask
  X.mapWindow display window
  runReaderT eventLoop Environment {..}
  X.freeFont display font

optionParser :: Parser String
optionParser = strOption (long "font" <> short 'f' <> value "fixed")

eventLoop :: ReaderT Environment IO ()
eventLoop = do
  Environment {..} <- ask
  liftIO $ do
    drawLines display window font content
    X.sync display False
  eventLoop

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

drawLines :: X.Display -> X.Window -> X.FontStruct -> [String] -> IO ()
drawLines display window font texts = do
  X.allocaXEvent $ X.nextEvent display
  gc   <- X.createGC display window
  let textPositions = verticalize texts font $ characterHeight font
  X.clearWindow display window
  X.setBackground display gc backgroundColor
  X.setForeground display gc textColor
  X.setFont display gc $ X.fontFromFontStruct font
  forM_ textPositions $ \PositionedText {..} ->
    X.drawString display window gc 5 ptY ptText
  X.freeGC display gc
