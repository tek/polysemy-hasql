module Polysemy.Hasql.Test.EnumTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Hasql.Column.DataColumn (tableStructure)
import Polysemy.Hasql.Data.DbType (DbType(Prim, Prod), Column(Column))

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

enumColTable :: Column
enumColTable =
  tableStructure @Auto @EnumCol

test_enumColTable :: UnitTest
test_enumColTable =
  runTestAuto do
    Column "enum_col" [qt|"enum_col"|] "enum_col" def (Prod [Column "e" [qt|"e"|] "text" def Prim]) === enumColTable

enumsColTable :: Column
enumsColTable =
  tableStructure @Auto @EnumsCol

test_enumsColTable :: UnitTest
test_enumsColTable =
  runTestAuto do
    Column "enums_col" [qt|"enums_col"|] "enums_col" def (Prod [Column "e" [qt|"e"|] "text[]" def Prim]) === enumsColTable
