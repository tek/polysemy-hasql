module Main where

import Polysemy.Db.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Db.Test.StatementTest (test_createStatement, test_selectStatement)
import Polysemy.Db.Test.TableTest (tableTests)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    tableTests,
    unitTest "column of an enum" test_enumColTable,
    unitTest "column of a list of enums" test_enumsColTable,
    unitTest "derive a select statement" test_selectStatement,
    unitTest "derive a create table statement" test_createStatement
  ]

main :: IO ()
main =
  defaultMain tests
