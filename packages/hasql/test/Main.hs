module Main where

import Polysemy.Hasql.Test.DeriveQuery.UnaSumNumberedTest (test_deriveQuery_UnaSumNumbered)
import Polysemy.Hasql.Test.EnumTest (test_enumColTable, test_enumsColTable)
import Polysemy.Hasql.Test.ParamTest ()
import Polysemy.Hasql.Test.PartialTest (test_partialTree, test_partialUpdateNestedSum, test_partialUpdateSum)
import Polysemy.Hasql.Test.RepTest (test_rep)
import Polysemy.Hasql.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Polysemy.Hasql.Test.StatementTest (statementTests)
import Polysemy.Hasql.Test.TableTest (tableTests)
import Polysemy.Hasql.Test.Tree.DeriveNewtypePartialTest (test_deriveNewtypePartial)
import Polysemy.Hasql.Test.Tree.DeriveProdTest (test_deriveProd)
import Polysemy.Hasql.Test.Tree.DeriveSumDbTest (test_deriveSumDb)
import Polysemy.Hasql.Test.Tree.DeriveSumFieldDbTest (test_deriveSumFieldDb)
import Polysemy.Hasql.Test.Tree.DeriveSumFieldTest (test_deriveSumField)
import Polysemy.Hasql.Test.Tree.DeriveSumIdDbTest (test_deriveSumIdDb)
import Polysemy.Hasql.Test.Tree.DeriveSumTest (test_deriveSum)
import Polysemy.Hasql.Test.Tree.DeriveUnitTest (test_deriveUnit)
import Polysemy.Hasql.Test.Tree.JsonTest (test_treeJson)
import Polysemy.Hasql.Test.Tree.LookupPartialTest (test_lookupPartial)
import Polysemy.Hasql.Test.Tree.MergePartialTest (test_mergePartial)
import Polysemy.Hasql.Test.UidSumFlattenTest (test_uidSumFlatten)
import Polysemy.Hasql.Test.WhereTest (test_where_Flatten_Sum)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    tableTests,
    statementTests,
    unitTest "esql quote without interpolation" test_sqlCodeNoInterpolation,
    unitTest "column of an enum" test_enumColTable,
    unitTest "column of a list of enums" test_enumsColTable,
    unitTest "derive db representations" test_rep,
    unitTest "partial tree update" test_partialTree,
    unitTest "partial sum update" test_partialUpdateSum,
    unitTest "partial nested sum update" test_partialUpdateNestedSum,
    unitTest "derive prod data tree" test_deriveProd,
    unitTest "derive sum data tree" test_deriveSum,
    unitTest "derive data tree with sum field" test_deriveSumField,
    unitTest "derive db tree" test_deriveSumDb,
    unitTest "derive db tree with sum field" test_deriveSumFieldDb,
    unitTest "derive db tree with sum id field" test_deriveSumIdDb,
    unitTest "partial tree json codec" test_treeJson,
    unitTest "derive db tree with sum field in Uid" test_uidSumFlatten,
    unitTest "derive a where clause with a prim query for an unary sum constructor" test_deriveQuery_UnaSumNumbered,
    unitTest "derive a partial tree with a newtype field" test_deriveNewtypePartial,
    unitTest "merge a partial tree into another" test_mergePartial,
    unitTest "extract a partial subtree" test_lookupPartial,
    unitTest "derive a unit prim" test_deriveUnit,
    unitTest "derive Where with a Flatten in one sum constructor" test_where_Flatten_Sum
  ]

main :: IO ()
main =
  defaultMain tests
