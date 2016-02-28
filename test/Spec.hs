import Test.HUnit

import qualified Scale.Tests

main :: IO Counts
main = runTestTT $ TestList
  [ Scale.Tests.tests
  ]
