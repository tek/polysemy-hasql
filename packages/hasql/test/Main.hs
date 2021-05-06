module Main where

import Polysemy.Hasql.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Hasql.Test.ParamTest ()
import Polysemy.Hasql.Test.Partial.DeriveSum (test_deriveSum)
import Polysemy.Hasql.Test.Partial.DeriveSumField (test_deriveSumField)
import Polysemy.Hasql.Test.Partial.DeriveSumFieldDb (test_deriveSumFieldDb)
import Polysemy.Hasql.Test.Partial.DeriveSumIdDb (test_deriveSumIdDb)
import Polysemy.Hasql.Test.PartialTest (test_partialTree, test_partialUpdateSum)
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
    unitTest "partial tree update" test_partialTree,
    unitTest "partial sum update" test_partialUpdateSum,
    unitTest "derive sum data tree" test_deriveSum,
    unitTest "derive data tree with sum field" test_deriveSumField,
    unitTest "derive db tree with sum field" test_deriveSumFieldDb,
    unitTest "derive db tree with sum id field" test_deriveSumIdDb
  ]

main :: IO ()
main =
  defaultMain tests
