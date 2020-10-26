module Polysemy.Db.Data.TableStructure where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.TableName (TableName)

data Column =
  Column {
    columnName :: Text,
    dataType :: Text,
    params :: ColumnOptions,
    customType :: Maybe CompositeType
  }
  deriving (Eq, Show)

data CompositeType =
  CompositeType {
    _name :: TableName,
    _index :: Column,
    _variants :: [TableStructure]
  }
  deriving (Eq, Show)

data TableStructure =
  TableStructure {
    _name :: TableName,
    _columns :: [Column]
  }
  deriving (Eq, Show)

makeClassy ''TableStructure
