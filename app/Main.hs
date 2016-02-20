{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle
import Turtle.Options.Scale (Scale, optScale, defScaleHelp)

parser :: Parser Scale
parser = optScale "scale" 's' defScaleHelp

main :: IO ()
main = do
  scale <- options "Turtle options example" parser
  print scale