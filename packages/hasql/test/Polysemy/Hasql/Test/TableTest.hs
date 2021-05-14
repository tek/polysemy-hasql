module Polysemy.Hasql.Test.TableTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Hasql.Table (missingColumns)
import Polysemy.Db.Text.DbIdentifier (dbIdentifier)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn(ExistingColumn))
import Polysemy.Hasql.Column.DataColumn (tableStructure)
import Polysemy.Hasql.Data.DbType (Column(Column), DbType(Prod, Prim))

data Rec =
  Rec {
    field1 :: Text,
    field2 :: Int
  }
  deriving (Eq, Show, Generic)

recTable :: Column
recTable =
  tableStructure @Auto @Rec

test_recColumns :: UnitTest
test_recColumns =
  runTestAuto do
    target === recTable
  where
    target =
      Column "rec" [text|"rec"|] "rec" def (Prod [
        Column "field1" [text|"field1"|] "text" def Prim,
        Column "field2" [text|"field2"|] "bigint" def Prim
      ])

data UuidCol =
  UuidCol { uuid :: UUID }
  deriving (Eq, Show, Generic)

uuidColTable :: Column
uuidColTable =
  tableStructure @Auto @UuidCol

test_uuidColTable :: UnitTest
test_uuidColTable =
  runTestAuto do
    target === uuidColTable
  where
    target =
      Column "uuid_col" [text|"uuid_col"|] "uuid_col" def (Prod [Column "uuid" [text|"uuid"|] "uuid" def Prim])

test_tableName :: UnitTest
test_tableName =
  runTestAuto do
    "rec_ii_ord" === (dbIdentifier "RecIIOrd")
    "camel_case" === (dbIdentifier "camelCase")

targetMissing :: [Column]
targetMissing =
  [Column "f4" [text|"f4"|] "text" def Prim]

updateColumnsExisting :: [ExistingColumn]
updateColumnsExisting =
  [
    ExistingColumn "f1" "bigint",
    ExistingColumn "f2" "uuid",
    ExistingColumn "f3" "text"
  ]

updateColumnsTarget :: [Column]
updateColumnsTarget =
  [
    Column "f1" [text|"f1"|] "bigint" def Prim,
    Column "f2" [text|"f2"|] "text" def Prim,
    Column "f4" [text|"f4"|] "text" def Prim
  ]

test_updateTasks :: UnitTest
test_updateTasks =
  runTestAuto do
    nonEmpty targetMissing === missingColumns updateColumnsExisting updateColumnsTarget

tableTests :: TestTree
tableTests =
  testGroup "table" [
    unitTest "derive columns" test_recColumns,
    unitTest "UUID column" test_uuidColTable,
    unitTest "table name identifier" test_tableName,
    unitTest "column updates" test_updateTasks
  ]
