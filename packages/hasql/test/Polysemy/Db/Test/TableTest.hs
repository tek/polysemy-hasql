module Polysemy.Db.Test.TableTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table (missingColumns)
import Polysemy.Hasql.Table.Identifier (dbIdentifier)
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

data Rec =
  Rec {
    field1 :: Text,
    field2 :: Int
  }
  deriving (Eq, Show)

deriveGeneric ''Rec

recTable :: TableStructure
recTable =
  genTableStructure @Auto @Rec

test_recColumns :: UnitTest
test_recColumns =
  runTestAuto do
    target === recTable
  where
    target =
      TableStructure "rec" (Columns (Column "field1" "text" def :| [Column "field2" "bigint" def]))

data UuidCol =
  UuidCol { uuid :: UUID }
  deriving (Eq, Show)

deriveGeneric ''UuidCol

uuidColTable :: TableStructure
uuidColTable =
  genTableStructure @Auto @UuidCol

test_uuidColTable :: UnitTest
test_uuidColTable =
  runTestAuto do
    TableStructure "uuid_col" (Columns (Column "uuid" "uuid" def :| [])) === uuidColTable

test_tableName :: UnitTest
test_tableName =
  runTestAuto do
    "rec_ii_ord" === (dbIdentifier "RecIIOrd")
    "camel_case" === (dbIdentifier "camelCase")

targetMissing :: [Column]
targetMissing =
  [Column "f4" "text" def]

updateColumnsExisting :: NonEmpty Column
updateColumnsExisting =
  Column "f1" "bigint" def :| [Column "f2" "uuid" def, Column "f3" "text" def]

updateColumnsTarget :: Columns
updateColumnsTarget =
  Columns (Column "f1" "bigint" def :| [Column "f2" "text" def, Column "f4" "text" def])

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
