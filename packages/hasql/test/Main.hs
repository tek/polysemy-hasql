module Main where

import Polysemy.Hasql.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Hasql.Test.ParamTest ()
import Polysemy.Hasql.Test.PartialTest (test_partialTree, test_partialUpdate)
import Polysemy.Hasql.Test.RepTest (test_rep)
import Polysemy.Hasql.Test.StatementTest (statementTests)
import Polysemy.Hasql.Test.TableTest (tableTests)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    tableTests,
    statementTests,
    unitTest "column of an enum" test_enumColTable,
    unitTest "column of a list of enums" test_enumsColTable,
    unitTest "derive db representations" test_rep,
    unitTest "partial adt update" test_partialUpdate,
    unitTest "partial tree update" test_partialTree
  ]

main :: IO ()
main =
  defaultMain tests
