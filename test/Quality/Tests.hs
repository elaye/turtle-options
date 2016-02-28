{-# LANGUAGE FlexibleContexts #-}
module Quality.Tests
( tests
) where

import Test.HUnit
import Text.Parsec (parse)

import Turtle.Options.Quality (quality)

qualityTest :: Test
qualityTest = TestList $ map testCase cases
  where
    cases = 
      [ ("verylow", 0.1)
      , ("low", 0.2)
      , ("mediumlow", 0.35)
      , ("medium", 0.5)
      , ("mediumhigh", 0.65)
      , ("high", 0.8)
      , ("veryhigh", 0.9)
      , ("best", 1)
      , ("15%", 0.15)
      ]
    testCase (str, res) = TestCase $ case (parse quality "Test" str) of
      Left err -> assertFailure $ show err
      Right r -> assertEqual "Quality parsing" res r

tests :: Test
tests = TestList 
  [ qualityTest
  ]