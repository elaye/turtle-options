{-# LANGUAGE FlexibleContexts #-}
module Timecode.Tests
( tests
) where

import Test.HUnit
import Text.Parsec (parse)

import Turtle.Options.Timecode (Timecode(..), timecode)

timecodeTest :: Test
timecodeTest = TestList $ map testCase cases
  where
    cases = 
      [ ("3", Timecode 0 0 3 0)
      , ("75", Timecode 0 1 15 0)
      , ("17:12", Timecode 0 17 12 0)
      , ("83:23", Timecode 1 23 23 0)
      , ("54:32:10", Timecode 54 32 10 0)
      , ("43.7", Timecode 0 0 43 700)
      , ("4:13.85", Timecode 0 4 13 850)
      , ("7:4:13.437", Timecode 7 4 13 437)
      , ("5.2150", Timecode 0 0 7 150)
      ]
    testCase (str, res) = TestCase $ case (parse timecode "Test" str) of
      Left err -> assertFailure $ show err
      Right r -> assertEqual "Timecode parsing" res r

tests :: Test
tests = TestList 
  [ timecodeTest
  ]