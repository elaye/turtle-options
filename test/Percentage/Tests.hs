{-# LANGUAGE FlexibleContexts #-}
module Percentage.Tests
( tests
) where

import Test.HUnit
import Text.Parsec (parse)

import Turtle.Options.Percentage (percentage)

percentageTest :: Test
percentageTest = TestList $ map testCase cases
  where
    cases = 
      [ ("23%", 0.23)
      ]
    testCase (str, res) = TestCase $ case (parse percentage "Test" str) of
      Left err -> assertFailure $ show err
      Right r -> assertEqual "Percentage parsing" res r

tests :: Test
tests = TestList 
  [ percentageTest
  ]