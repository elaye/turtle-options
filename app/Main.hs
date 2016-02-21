{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Turtle.Options.Scale (Scale, optScale, defScaleHelp)
import Turtle.Options.Percentage (optPercentage, defPercentageHelp)
import Turtle.Options.Quality (optQuality, defQualityHelp)
import Turtle.Options.Timecode (Timecode, optTimecode, defTimecodeHelp)

parser :: Parser (Scale, Float, Float, Timecode)
parser = (,,,) <$> optScale "scale" 's' defScaleHelp
            <*> optPercentage "percentage" 'p' defPercentageHelp
            <*> optQuality "quality" 'q' defQualityHelp
            <*> optTimecode "timecode" 't' defTimecodeHelp 

main :: IO ()
main = do
  (scale, percent, quality, timecode) <- options "Turtle options example" parser
  putStrLn $ "Scale " ++ (show scale)
  putStrLn $ "Percent " ++ (show percent)
  putStrLn $ "Quality " ++ (show quality)
  putStrLn $ "Timecode " ++ (show timecode)