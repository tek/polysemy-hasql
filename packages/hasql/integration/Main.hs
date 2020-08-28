module Main where

import Polysemy.Db.Test (unitTest)
import Polysemy.Db.Test.InitTest (test_initTable)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "initialize an outdated table" test_initTable
  ]

main :: IO ()
main =
  defaultMain tests
