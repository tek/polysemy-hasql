module EnumTest where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table.TableStructure (genTableStructure)
import Polysemy.Db.Test (UnitTest, (===))

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
  genTableStructure @EnumCol @Auto

test_enumColTable :: UnitTest
test_enumColTable =
  TableStructure "enum_col" (Columns (Column "e" "text" def :| [])) === enumColTable

enumsColTable :: TableStructure
enumsColTable =
  genTableStructure @EnumsCol @Auto

test_enumsColTable :: UnitTest
test_enumsColTable =
  TableStructure "enums_col" (Columns (Column "e" "text[]" def :| [])) === enumsColTable
