{-# LANGUAGE OverloadedStrings #-}

module Turtle.Options.Percentage
( optPercentage
, defPercentageHelp
) where

import Turtle (ArgName, ShortName, HelpMessage, opt)
import Data.Optional (Optional)
import qualified Turtle
import qualified Data.Text as Text

import Control.Applicative ((<$>))

import Text.Parsec

import Turtle.Options.Parsers (Parser, percent, float)

defPercentageHelp :: Optional HelpMessage
defPercentageHelp = "Percentage: can be a positive or negative percentage (-43%), represented with a float." 

percentage :: Parser Float
percentage = try ((/100) <$> percent)

readPercentage :: String -> Maybe Float
readPercentage str = case (parse percentage "Percentage (Float)" str) of
  Left err -> error $ "Error parsing percentage: " ++ (show err)
  Right s -> Just s

optPercentage :: ArgName -> ShortName -> Optional HelpMessage -> Turtle.Parser Float
optPercentage = opt (readPercentage . Text.unpack)