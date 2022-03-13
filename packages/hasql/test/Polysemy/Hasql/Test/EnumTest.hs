module Polysemy.Hasql.Test.EnumTest where

import Exon (exon)
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Hasql.Data.DbType (Column (Column), DbType (Prim, Prod), TypeName (CompositeTypeName))
import Polysemy.Hasql.Table.DataColumn (tableStructure)

data En =
  One
  |
  Two
  |
  Three
  deriving stock (Eq, Show, Generic)

data EnumCol =
  EnumCol { e :: En }
  deriving stock (Eq, Show, Generic)

data EnumsCol =
  EnumsCol { e :: NonEmpty En }
  deriving stock (Eq, Show, Generic)

enumColTable :: Column
enumColTable =
  tableStructure @Auto @EnumCol

test_enumColTable :: UnitTest
test_enumColTable =
  runTestAuto do
    Column "enum_col" (fromString [exon|"enum_col"|]) (CompositeTypeName "enum_col") def (Prod [Column "e" (fromString [exon|"e"|]) "text" def Prim]) === enumColTable

enumsColTable :: Column
enumsColTable =
  tableStructure @Auto @EnumsCol

test_enumsColTable :: UnitTest
test_enumsColTable =
  runTestAuto do
    Column "enums_col" (fromString [exon|"enums_col"|]) (CompositeTypeName "enums_col") def (Prod [Column "e" (fromString [exon|"e"|]) "text[]" def Prim]) === enumsColTable
