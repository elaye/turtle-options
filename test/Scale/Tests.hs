{-# LANGUAGE FlexibleContexts #-}
module Scale.Tests
( tests
) where

import Test.HUnit
import Text.Parsec (parse)

import Turtle.Options.Scale (Scale(..), scale)

scaleTest :: Test
scaleTest = TestList $ map testCase cases
  where
    cases = 
      [ ("5", Percentage 5)
      , ("10%", Percentage 0.1)
      , ("480x320", Size (480, 320))
      , ("x480", Height 480)
      , ("x320", Height 320)
      ]
    testCase (str, res) = TestCase $ case (parse scale "Test" str) of
      Left err -> assertFailure $ show err
      Right r -> assertEqual "Scale parsing" res r

tests :: Test
tests = TestList 
  [ scaleTest
  ]