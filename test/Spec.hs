import Test.HUnit

import qualified Scale.Tests
import qualified Quality.Tests

main :: IO Counts
main = runTestTT $ TestList
  [ Scale.Tests.tests
  , Quality.Tests.tests
  ]
