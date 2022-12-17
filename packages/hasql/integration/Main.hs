module Main where

import Polysemy.Hasql.Test.ArrayTest (test_arrayField)
import Polysemy.Hasql.Test.AtomicStateTest (test_atomicStateDb)
import Polysemy.Hasql.Test.DslTest (test_dsl)
import Polysemy.Hasql.Test.JsonTest (test_json)
-- import Polysemy.Hasql.Test.QueryTest (test_query)
import Polysemy.Hasql.Test.QueueTest (test_queue)
import Polysemy.Hasql.Test.RetryTest (test_retry)
-- import Polysemy.Hasql.Test.SingletonTest (test_singletonDb)
-- import Polysemy.Hasql.Test.StoreUpdateTest (test_partialDbUpdate)
-- import Polysemy.Hasql.Test.SumFieldTest (test_multiSum, test_sumField, test_sumId)
-- import Polysemy.Hasql.Test.SumTableTest (test_sumTable)
import Polysemy.Hasql.Test.TransactionTest (test_transaction)
-- import Polysemy.Hasql.Test.UnarySumTest (test_unarySum)
-- import Polysemy.Hasql.Test.UpsertTest (test_upsert)
import Polysemy.Hasql.Test.WithInitTest (test_withInit)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    -- unitTest "schema for a singleton db" test_singletonDb,
    unitTest "array db column" test_arrayField,
    -- unitTest "query" test_query,
    -- unitTest "sum field" test_sumField,
    -- unitTest "sum field as id column" test_sumId,
    -- unitTest "same sum type in multiple tables" test_multiSum,
    -- unitTest "primary key with PK" test_pk,
    -- unitTest "upsert" test_upsert,
    unitTest "queue with notifications" test_queue,
    unitTest "retry on error" test_retry,
    unitTest "atomic state as table" test_atomicStateDb,
    -- unitTest "partial record update" test_partialDbUpdate,
    -- unitTest "sum table" test_sumTable,
    unitTest "json field" test_json,
    -- unitTest "unary sum" test_unarySum,
    -- unitTest "delete all in an empty table" test_deleteEmpty,
    test_dsl,
    unitTest "transaction" test_transaction,
    unitTest "init hook" test_withInit
  ]

main :: IO ()
main =
  defaultMain tests
