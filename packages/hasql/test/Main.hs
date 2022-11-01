module Main where

import Polysemy.Hasql.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "sql quote without interpolation" test_sqlCodeNoInterpolation
  ]

main :: IO ()
main =
  defaultMain tests
