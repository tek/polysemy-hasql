module Main where

import Sqel.Test.ErrorTest (test_errors)
import Sqel.Test.HasGeneric (test_hasGeneric)
import Sqel.Test.MigrationTest (test_migration)
import Sqel.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Sqel.Test.StatementTest (test_statement)
import Sqel.Test.TableSchemaTest (test_tableSchema)
import Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "table schema" test_tableSchema,
    unitTest "migration" test_migration,
    test_errors,
    test_statement,
    unitTest "sql quote without interpolation" test_sqlCodeNoInterpolation,
    unitTest "HasGeneric" test_hasGeneric
  ]

main :: IO ()
main =
  defaultMain tests
