module Polysemy.Hasql.Data.DbType where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Text.Quote (dquote)

data DbType =
  Prim
  |
  Prod { cols :: [Column] }
  |
  Sum { cols :: [Column] }
  deriving (Eq, Show)

newtype Name =
  Name { unName :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving newtype (IsString)

newtype Selector =
  Selector { unSelector :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving newtype (IsString)

nameSelector :: Text -> Selector
nameSelector =
  Selector . dquote

data Column =
  Column {
    _name :: Name,
    _selector :: Selector,
    _tpe :: Text,
    _options :: ColumnOptions,
    _dbType :: DbType
  }
  deriving (Eq, Show)

makeClassy ''Column
