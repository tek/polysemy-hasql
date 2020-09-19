module Main where

import Polysemy.Db.Test.ArrayTest (test_arrayField)
import Polysemy.Db.Test.InitTest (test_initTable)
import Polysemy.Db.Test.QueryTest (test_query)
import Polysemy.Db.Test.SingletonTest (test_singletonDb)
import Polysemy.Db.Test.SumFieldTest (test_sumField)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "initialize an outdated table" test_initTable,
    unitTest "schema for a singleton db" test_singletonDb,
    unitTest "array db column" test_arrayField,
    unitTest "query" test_query,
    unitTest "sum field" test_sumField
  ]

main :: IO ()
main =
  defaultMain tests
