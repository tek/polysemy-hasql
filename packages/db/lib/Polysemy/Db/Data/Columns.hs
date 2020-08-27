module Polysemy.Db.Data.Columns where

import Polysemy.Db.Data.ColumnParams (ColumnParams)

data Column =
  Column {
    name :: Text,
    dataType :: Text,
    params :: ColumnParams
  }
  deriving (Eq, Show)

newtype Columns =
  Columns { unColumns :: (NonEmpty Column) }
  deriving (Eq, Show)
