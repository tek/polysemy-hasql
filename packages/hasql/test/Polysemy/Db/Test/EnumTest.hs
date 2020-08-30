module Polysemy.Db.Test.EnumTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data En =
  One
  |
  Two
  |
  Three
  deriving (Eq, Show, Generic)

deriveGeneric ''En

data EnumCol =
  EnumCol { e :: En }
  deriving (Eq, Show)

deriveGeneric ''EnumCol

data EnumsCol =
  EnumsCol { e :: NonEmpty En }
  deriving (Eq, Show)

deriveGeneric ''EnumsCol

enumColTable :: TableStructure
enumColTable =
  genTableStructure @Auto @EnumCol

test_enumColTable :: UnitTest
test_enumColTable =
  runTestAuto do
    TableStructure "enum_col" (Columns (Column "e" "text" def :| [])) === enumColTable

enumsColTable :: TableStructure
enumsColTable =
  genTableStructure @Auto @EnumsCol

test_enumsColTable :: UnitTest
test_enumsColTable =
  runTestAuto do
    TableStructure "enums_col" (Columns (Column "e" "text[]" def :| [])) === enumsColTable
