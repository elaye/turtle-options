{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Turtle.Options.Scale (Scale, optScale, defScaleHelp)
import Turtle.Options.Percentage (optPercentage, defPercentageHelp)
import Turtle.Options.Quality (optQuality, defQualityHelp)

parser :: Parser (Scale, Float, Float)
parser = (,,) <$> optScale "scale" 's' defScaleHelp
            <*> optPercentage "percentage" 'p' defPercentageHelp
            <*> optQuality "quality" 'q' defQualityHelp

main :: IO ()
main = do
  (scale, percent, quality) <- options "Turtle options example" parser
  putStrLn $ "Scale " ++ (show scale)
  putStrLn $ "Percent " ++ (show percent)
  putStrLn $ "Quality " ++ (show quality)