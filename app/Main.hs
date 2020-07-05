{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader   (ReaderT, ask, runReaderT, local)
import Data.Bits              ((.|.))
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
  { display      :: X.Display
  , root         :: X.Window
  , window       :: X.Window
  , content      :: [String]
  , font         :: X.FontStruct
  , scrollOffset :: X.Position
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
  window       <- X.createSimpleWindow display root 0 0 250 250 0 (toPixel flavor) (toPixel flavor)
  scrollOffset <- pure 0
  X.selectInput display window $ X.exposureMask .|. X.buttonPressMask
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
  event <- liftIO $ X.allocaXEvent $
    \event -> X.nextEvent display event >> pure event

  liftIO $ do
    drawLines display window font scrollOffset content
    X.sync display False

  maybeButton <- liftIO $ X.get_EventType event >>= \type_ -> if 
    | type_ == X.buttonPress -> Just <$> eventButton <$> X.get_ButtonEvent event
    | otherwise              -> pure Nothing

  case maybeButton of
    Just 4 -> local (\e -> e { scrollOffset = scrollOffset - 10 }) eventLoop
    Just 5 -> local (\e -> e { scrollOffset = scrollOffset + 10 }) eventLoop
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

drawLines :: X.Display -> X.Window -> X.FontStruct -> X.Position -> [String] -> IO ()
drawLines display window font origin texts = do
  gc   <- X.createGC display window
  let textPositions = verticalize texts font $ origin + characterHeight font
  X.clearWindow display window
  X.setForeground display gc textColor
  X.setFont display gc $ X.fontFromFontStruct font
  forM_ textPositions $ \PositionedText {..} ->
    X.drawString display window gc 5 ptY ptText
  X.freeGC display gc
