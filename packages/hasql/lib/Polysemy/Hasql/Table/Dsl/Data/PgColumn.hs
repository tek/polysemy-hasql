module Polysemy.Hasql.Table.Dsl.Data.PgColumn where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions)

newtype PgPrimName =
  PgPrimName { unPgPrimName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype PgProdName =
  PgProdName { unPgProdName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype PgColumnName =
  PgColumnName { unPgColumnName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

-- TODO use PgColumnName
data PgType a =
  PgTypePrim PgPrimName
  |
  PgTypeProd PgProdName [(Text, PgField ())]
  deriving stock (Eq, Show)

instance IsString (PgType a) where
  fromString s =
    PgTypePrim (fromString s)

data PgColumn a =
  PgColumn {
    colType :: PgType a,
    options :: ColumnOptions
  }
  deriving stock (Eq, Show, Generic)

instance IsString (PgColumn a) where
  fromString s =
    PgColumn (fromString s) mempty

data PgField a =
  PgFieldColumn (PgColumn a)
  |
  PgFieldFlatten PgProdName [(Text, PgField ())]
  deriving stock (Eq, Show, Generic)

instance IsString (PgField a) where
  fromString s =
    PgFieldColumn (fromString s)

overOptions :: (ColumnOptions -> ColumnOptions) -> PgField a -> PgField a
overOptions f = \case
  PgFieldColumn col ->
    PgFieldColumn (col & #options %~ f)
  PgFieldFlatten n cols ->
    PgFieldFlatten n cols

addOptions :: ColumnOptions -> PgField a -> PgField a
addOptions o =
  overOptions (<> o)
