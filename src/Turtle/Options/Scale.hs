{-# LANGUAGE OverloadedStrings #-}

module Turtle.Options.Scale
( Scale(..)
, optScale
, defScaleHelp
, scale
) where

import Turtle (ArgName, ShortName, HelpMessage, opt)
import Data.Optional (Optional)
import qualified Turtle
import qualified Data.Text as Text
import Control.Applicative ((<$>), (<*>), (*>), (<*))

import Text.Parsec

import Turtle.Options.Parsers (Parser, percent, float)

data Scale =
  Percentage Float
  | Size (Int, Int)
  | Width Int
  | Height Int
  deriving (Eq, Show)

defScaleHelp :: Optional HelpMessage
defScaleHelp = "Scale option. SCALE can be a percentage (20%), a size (480x320), a width (480x) or a height (x320)"

width :: Parser Scale
width = Width . read <$> try (many1 digit <* char 'x')

height :: Parser Scale
height = Height . read <$> try (char 'x' *> many1 digit)

size :: Parser Scale
size = Size <$> try (do
  w <- read <$> many1 digit <* char 'x'
  h <- read <$> many1 digit
  return (w, h))

percentage :: Parser Scale
percentage = choice [try p, f]
  where 
    f = do
      v <- float 
      case v < 0 of
        True -> error "Error parsing scale: can't have a negative scale"
        False -> return $ Percentage v
    p = do
      v <- percent
      case v < 0 of
        True -> error "Error parsing scale percentage: can't have a negative value"
        False -> return $ Percentage v

scale :: Parser Scale
scale = choice [size, percentage, width, height]

readScale :: String -> Maybe Scale
readScale str = case (parse scale "Scale" str) of
  Left err -> error $ "Error parsing scale: " ++ (show err)
  Right s -> Just s

optScale :: ArgName -> ShortName -> Optional HelpMessage -> Turtle.Parser Scale
optScale = opt (readScale . Text.unpack)
