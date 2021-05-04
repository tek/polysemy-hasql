module Main where

import Polysemy.Hasql.Test.ArrayTest (test_arrayField)
import Polysemy.Hasql.Test.AtomicState (test_atomicStateDb)
import Polysemy.Hasql.Test.InitTest (test_initTable)
import Polysemy.Hasql.Test.PKTest (test_pk)
import Polysemy.Hasql.Test.PartialTest (test_partialDbUpdate)
import Polysemy.Hasql.Test.QueryTest (test_query)
import Polysemy.Hasql.Test.QueueTest (test_queue)
import Polysemy.Hasql.Test.SingletonTest (test_singletonDb)
import Polysemy.Hasql.Test.SumFieldTest (test_multiSum, test_sumField, test_sumId)
import Polysemy.Hasql.Test.SumTableTest (test_unaryVariants)
import Polysemy.Hasql.Test.UpsertTest (test_upsert)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "initialize an outdated table" test_initTable,
    unitTest "schema for a singleton db" test_singletonDb,
    unitTest "array db column" test_arrayField,
    unitTest "query" test_query,
    unitTest "sum field" test_sumField,
    unitTest "sum field as id column" test_sumId,
    unitTest "same sum type in multiple tables" test_multiSum,
    unitTest "primary key with PK" test_pk,
    unitTest "upsert" test_upsert,
    unitTest "queue with notifications" test_queue,
    unitTest "atomic state as table" test_atomicStateDb,
    unitTest "partial record update" test_partialDbUpdate,
    unitTest "unary sum variants" test_unaryVariants
  ]

main :: IO ()
main =
  defaultMain tests
