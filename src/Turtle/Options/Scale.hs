{-# LANGUAGE OverloadedStrings #-}

module Turtle.Options.Scale
( Scale
, optScale
, defScaleHelp
) where

import Turtle (ArgName, ShortName, HelpMessage, opt)
import Data.Optional (Optional)
import qualified Turtle
import qualified Data.Text as Text

import Text.Parsec

data Scale =
  Percent Int
  | Size (Int, Int)
  | Width Int
  | Height Int
  deriving (Eq, Show)

type Parser = Parsec String ()

defScaleHelp :: Optional HelpMessage
defScaleHelp = "Scale option. SCALE can be a percentage (20%), a size (480x320), a width (480x) or a height (x320)"

percent :: Parser Scale
percent = Percent . read <$> try (many1 digit <* char '%')

width :: Parser Scale
width = Width . read <$> try (many1 digit <* char 'x')

height :: Parser Scale
height = Height . read <$> try (char 'x' *> many1 digit)

size :: Parser Scale
size = Size <$> try (do
  w <- read <$> many1 digit <* char 'x'
  h <- read <$> many1 digit
  return (w, h))

scale :: Parser Scale
scale = choice [size, percent, width, height]

readScale :: String -> Maybe Scale
readScale str = case (parse scale "Scale" str) of
  Left _ -> Nothing
  Right s -> Just s

optScale :: ArgName -> ShortName -> Optional HelpMessage -> Turtle.Parser Scale
optScale = opt (readScale . Text.unpack)