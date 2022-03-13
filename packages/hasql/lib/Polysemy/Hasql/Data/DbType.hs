module Polysemy.Hasql.Data.DbType where

import Control.Lens (makeClassy)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Text.Quote (dquote)

import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))

data DbType =
  Prim
  |
  Prod { cols :: [Column] }
  |
  Sum { sumCols :: Column }
  deriving stock (Eq, Show)

newtype Name =
  Name { unName :: Text }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

newtype Selector =
  Selector { unSelector :: SqlCode }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

data TypeName =
  PrimTypeName { unTypeName :: Text }
  |
  CompositeTypeName { unTypeName :: Text }
  deriving stock (Eq, Show, Generic)

instance IsString TypeName where
  fromString =
    PrimTypeName . toText

textSelector :: Text -> Selector
textSelector =
  Selector . SqlCode

nameSelector :: Text -> Selector
nameSelector =
  textSelector . dquote

data Column =
  Column {
    _name :: Name,
    _selector :: Selector,
    _tpe :: TypeName,
    _options :: ColumnOptions,
    _dbType :: DbType
  }
  deriving stock (Eq, Show)

makeClassy ''Column
