import Test.HUnit

import qualified Scale.Tests
import qualified Quality.Tests
import qualified Percentage.Tests
import qualified Timecode.Tests

main :: IO Counts
main = runTestTT $ TestList
  [ Scale.Tests.tests
  , Quality.Tests.tests
  , Percentage.Tests.tests
  , Timecode.Tests.tests
  ]
