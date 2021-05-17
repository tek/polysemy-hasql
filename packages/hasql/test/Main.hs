module Main where

import Polysemy.Hasql.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Hasql.Test.ParamTest ()
import Polysemy.Hasql.Test.PartialTest (test_partialTree, test_partialUpdateSum)
import Polysemy.Hasql.Test.RepTest (test_rep)
import Polysemy.Hasql.Test.StatementTest (statementTests)
import Polysemy.Hasql.Test.TableTest (tableTests)
import Polysemy.Hasql.Test.Tree.DeriveProd (test_deriveProd)
import Polysemy.Hasql.Test.Tree.DeriveSum (test_deriveSum)
import Polysemy.Hasql.Test.Tree.DeriveSumDb (test_deriveSumDb)
import Polysemy.Hasql.Test.Tree.DeriveSumField (test_deriveSumField)
import Polysemy.Hasql.Test.Tree.DeriveSumFieldDb (test_deriveSumFieldDb)
import Polysemy.Hasql.Test.Tree.DeriveSumIdDb (test_deriveSumIdDb)
import Polysemy.Hasql.Test.Tree.JsonTest (test_treeJson)
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
    unitTest "derive prod data tree" test_deriveProd,
    unitTest "derive sum data tree" test_deriveSum,
    unitTest "derive data tree with sum field" test_deriveSumField,
    unitTest "derive db tree" test_deriveSumDb,
    unitTest "derive db tree with sum field" test_deriveSumFieldDb,
    unitTest "derive db tree with sum id field" test_deriveSumIdDb,
    unitTest "partial tree json codec" test_treeJson
  ]

main :: IO ()
main =
  defaultMain tests
