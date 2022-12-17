module Main where

import Polysemy.Hasql.Test.MigrationTest (test_migrationConsistency, test_migrationErrors)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "migration errors" test_migrationErrors,
    unitTest "migration consistency" test_migrationConsistency
  ]

main :: IO ()
main =
  defaultMain tests
