module Polysemy.Hasql.Test.DslTest where

import Polysemy.Test (unitTest)
import Prelude hiding (sum)
import Test.Tasty (TestTree, testGroup)

import Polysemy.Hasql.Test.Dsl.MigrationTest (test_dslMigration)
import Polysemy.Hasql.Test.Dsl.QueryTest (test_dslQuery)
import Polysemy.Hasql.Test.Dsl.SimpleQueryTest (test_dslSimpleQuery)
import Polysemy.Hasql.Test.Dsl.SumTest (test_dslSum)
import Polysemy.Hasql.Test.Dsl.TableSchemaTest (test_dslTableSchema)
import Polysemy.Hasql.Test.Dsl.UnaryConTest (test_dslUnaryCon)

test_dsl :: TestTree
test_dsl =
  testGroup "dsl" [
    unitTest "simple query" test_dslSimpleQuery,
    unitTest "query" test_dslQuery,
    unitTest "sum" test_dslSum,
    unitTest "sum with unary constructor" test_dslUnaryCon,
    unitTest "table schema" test_dslTableSchema,
    unitTest "migration" test_dslMigration
  ]
