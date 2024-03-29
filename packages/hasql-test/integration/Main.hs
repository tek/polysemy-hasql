module Main where

import Polysemy.Hasql.Test.ArrayTest (test_arrayField)
import Polysemy.Hasql.Test.AtomicStateTest (test_atomicStateDb)
import Polysemy.Hasql.Test.JsonTest (test_json)
import Polysemy.Hasql.Test.MigrationTest (test_migration)
import Polysemy.Hasql.Test.QueryTest (test_query, test_queryId)
import Polysemy.Hasql.Test.QueueTest (test_queue)
import Polysemy.Hasql.Test.RetryTest (test_retry)
import Polysemy.Hasql.Test.SimpleQueryTest (test_simpleQuery)
import Polysemy.Hasql.Test.SumQueryTest (test_sumQuery)
import Polysemy.Hasql.Test.SumTest (test_sum)
import Polysemy.Hasql.Test.TransactionTest (test_transaction)
import Polysemy.Hasql.Test.TransformMigrationTest (test_transformMigration)
import Polysemy.Hasql.Test.UnaryConTest (test_unaryCon)
import Polysemy.Hasql.Test.WithInitTest (test_withInit)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "array db column" test_arrayField,
    unitTest "queue with notifications" test_queue,
    unitTest "retry on error" test_retry,
    unitTest "atomic state as table" test_atomicStateDb,
    unitTest "json field" test_json,
    unitTest "simple query" test_simpleQuery,
    unitTest "query" test_query,
    unitTest "query" test_queryId,
    unitTest "sum" test_sum,
    unitTest "sum query" test_sumQuery,
    unitTest "sum with unary constructor" test_unaryCon,
    unitTest "migration" test_migration,
    unitTest "transaction" test_transaction,
    unitTest "init hook" test_withInit,
    unitTest "migration with transformation" test_transformMigration
  ]

main :: IO ()
main =
  defaultMain tests
