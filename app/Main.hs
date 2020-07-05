{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module Main where

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ReaderT, ask, runReaderT)
import Data.Char              (toLower)
import Data.Int               (Int32)
import Options.Applicative
    (Parser, execParser, helper, info, long, maybeReader, option, short,
    strOption, value, (<**>))
import System.Environment     (lookupEnv)

import qualified Graphics.X11 as X

textColor :: X.Pixel
textColor = 0xFF_11_2B_3A

data Flavor = Banana
  | Cherry
  | Apple
  | Blueberry

readFlavor :: String -> Flavor
readFlavor "banana"    = Banana
readFlavor "cherry"    = Cherry
readFlavor "apple"     = Apple
readFlavor "blueberry" = Blueberry
readFlavor _           = Banana

instance Enum Flavor where
  fromEnum Banana    = 0xFF_FF_ED_BB
  fromEnum Cherry    = 0xFF_FF_CB_BB
  fromEnum Apple     = 0xFF_EF_FF_BB
  fromEnum Blueberry = 0xFF_BB_CD_FF

  toEnum 0xFF_FF_ED_BB = Banana
  toEnum 0xFF_FF_CB_BB = Cherry
  toEnum 0xFF_EF_FF_BB = Apple
  toEnum 0xFF_BB_CD_FF = Blueberry
  toEnum _             = Banana

toPixel :: Flavor -> X.Pixel
toPixel = fromIntegral . fromEnum

data Arguments = Arguments
  { fontName :: String
  , flavor   :: Flavor
  }

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
  Arguments {..} <- execParser $ info (optionParser <**> helper) mempty
  display  <- maybe (error "Couldn't open display!") X.openDisplay =<< lookupEnv "DISPLAY"
  font     <- X.loadQueryFont display fontName
  content  <- lines <$> getContents
  root     <- X.rootWindow display 0
  window   <- X.createSimpleWindow display root 0 0 250 250 0 (toPixel flavor) (toPixel flavor)
  X.selectInput display window X.exposureMask
  X.mapWindow display window
  runReaderT eventLoop Environment {..}
  X.freeFont display font

optionParser :: Parser Arguments
optionParser = do
  fontName <- strOption (long "font" <> short 'f' <> value "fixed")
  flavor   <- option (maybeReader (pure . readFlavor . map toLower)) (long "flavor" <> short 'c' <> value Banana)
  pure Arguments {..}

eventLoop :: ReaderT Environment IO ()
eventLoop = do
  Environment {..} <- ask
  liftIO $ do
    X.allocaXEvent $ X.nextEvent display
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
  gc   <- X.createGC display window
  let textPositions = verticalize texts font $ characterHeight font
  X.clearWindow display window
  X.setForeground display gc textColor
  X.setFont display gc $ X.fontFromFontStruct font
  forM_ textPositions $ \PositionedText {..} ->
    X.drawString display window gc 5 ptY ptText
  X.freeGC display gc
