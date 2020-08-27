module TableTest where

-- import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
-- import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
-- import Polysemy.Db.Table (missingColumns)
-- import Polysemy.Db.Table.Identifier (dbIdentifier)
-- import Polysemy.Db.Table.TableStructure (genTableStructure)
-- import Polysemy.Db.Test (UnitTest, (===))

-- data Rec =
--   Rec {
--     field1 :: Text,
--     field2 :: Int
--   }
--   deriving (Eq, Show)

-- deriveGeneric ''Rec

-- recTable :: TableStructure
-- recTable =
--   genTableStructure @Rec

-- test_recColumns :: UnitTest
-- test_recColumns =
--   target === recTable
--   where
--     target =
--       TableStructure "rec" (Columns (Column "field1" "text" def :| [Column "field2" "bigint" def]))

-- data UuidCol =
--   UuidCol { uuid :: UUID }
--   deriving (Eq, Show)

-- deriveGeneric ''UuidCol

-- uuidColTable :: TableStructure
-- uuidColTable =
--   genTableStructure @UuidCol

-- test_uuidColTable :: UnitTest
-- test_uuidColTable =
--   TableStructure "uuid_col" (Columns (Column "uuid" "uuid" def :| [])) === uuidColTable

-- test_tableName :: UnitTest
-- test_tableName = do
--   "rec_ii_ord" === (dbIdentifier "RecIIOrd")
--   "camel_case" === (dbIdentifier "camelCase")

-- targetMissing :: [Column]
-- targetMissing =
--   [Column "f4" "text" def]

-- updateColumnsExisting :: NonEmpty Column
-- updateColumnsExisting =
--   Column "f1" "bigint" def :| [Column "f2" "uuid" def, Column "f3" "text" def]

-- updateColumnsTarget :: Columns
-- updateColumnsTarget =
--   Columns (Column "f1" "bigint" def :| [Column "f2" "text" def, Column "f4" "text" def])

-- test_updateTasks :: UnitTest
-- test_updateTasks =
--   nonEmpty targetMissing === missingColumns updateColumnsExisting updateColumnsTarget
