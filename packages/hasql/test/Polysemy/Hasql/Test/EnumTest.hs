module Polysemy.Hasql.Test.EnumTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.TableStructure (Column(Column), TableStructure(TableStructure))
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data En =
  One
  |
  Two
  |
  Three
  deriving (Eq, Show, Generic)

data EnumCol =
  EnumCol { e :: En }
  deriving (Eq, Show, Generic)

data EnumsCol =
  EnumsCol { e :: NonEmpty En }
  deriving (Eq, Show, Generic)

enumColTable :: TableStructure
enumColTable =
  genTableStructure @Auto @EnumCol

test_enumColTable :: UnitTest
test_enumColTable =
  runTestAuto do
    TableStructure "enum_col" [Column "e" "text" def Nothing] === enumColTable

enumsColTable :: TableStructure
enumsColTable =
  genTableStructure @Auto @EnumsCol

test_enumsColTable :: UnitTest
test_enumsColTable =
  runTestAuto do
    TableStructure "enums_col" [Column "e" "text[]" def Nothing] === enumsColTable
