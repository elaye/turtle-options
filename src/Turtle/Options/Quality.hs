{-# LANGUAGE OverloadedStrings #-}

module Turtle.Options.Quality
( optQuality
, defQualityHelp
) where

import Turtle (ArgName, ShortName, HelpMessage, opt)
import Data.Optional (Optional)
import qualified Turtle
import qualified Data.Text as Text

import Text.Parsec
import Text.ParserCombinators.Parsec.Error (Message(..), newErrorMessage)
import Text.Parsec.Pos (initialPos)

import Turtle.Options.Parsers (Parser, percent, float)

defQualityHelp :: Optional HelpMessage
defQualityHelp = "Quality option. QUALITY can be a percentage (20%) or a keyword: verylow / low / mediumlow / medium / mediumhigh / high / best."

qualitySettings = 
  [ ("verylow", 0.1)
  , ("low", 0.2)
  , ("mediumlow", 0.35)
  , ("medium", 0.5)
  , ("mediumhigh", 0.65)
  , ("high", 0.8)
  , ("best", 1)
  ]

keyword :: Parser Float
keyword = do
  key <- many1 lower
  let q = lookup key qualitySettings
  case q of
    Nothing -> error ("The keyword '" ++ key ++ "' is not a valid quality setting")
    Just v -> return v

quality :: Parser Float
quality = try keyword <|> percent

readQuality :: String -> Maybe Float
readQuality str = case (parse quality "Quality" str) of
  Left err -> error $ "Error parsing quality: " ++ (show err)
  Right s -> Just s

optQuality :: ArgName -> ShortName -> Optional HelpMessage -> Turtle.Parser Float
optQuality = opt (readQuality . Text.unpack)