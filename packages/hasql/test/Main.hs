module Main where

import Polysemy.Hasql.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Hasql.Test.RepTest (test_rep)
import Polysemy.Hasql.Test.StatementTest (test_createStatement, test_insertStatement, test_selectStatement)
import Polysemy.Hasql.Test.TableTest (tableTests)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    tableTests,
    unitTest "column of an enum" test_enumColTable,
    unitTest "column of a list of enums" test_enumsColTable,
    unitTest "derive a select statement" test_selectStatement,
    unitTest "derive a select statement" test_insertStatement,
    unitTest "derive a create table statement" test_createStatement,
    unitTest "derive db representations" test_rep
  ]

main :: IO ()
main =
  defaultMain tests
