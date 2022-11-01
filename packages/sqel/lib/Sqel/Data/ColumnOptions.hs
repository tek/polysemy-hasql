module Sqel.Data.ColumnOptions where

import Prettyprinter (Pretty (pretty), list)

import Sqel.Data.Sql (Sql, sql)

data ColumnOptions =
  ColumnOptions {
     unique :: Bool,
     notNull :: Bool,
     primaryKey :: Bool
  }
  deriving stock (Eq, Show, Ord, Generic)

instance Default ColumnOptions where
  def =
    ColumnOptions False True False

instance Semigroup ColumnOptions where
  ColumnOptions u1 n1 p1 <> ColumnOptions u2 n2 p2 =
    ColumnOptions (u1 || u2) (n1 && n2) (p1 || p2)

instance Monoid ColumnOptions where
  mempty =
    ColumnOptions False True False

instance Pretty ColumnOptions where
  pretty ColumnOptions {..} =
    case filter snd [("unique", unique), ("nullable", not notNull), ("primary", primaryKey)] of
      [] -> ""
      os -> list (fst <$> os)

format :: ColumnOptions -> Sql
format (ColumnOptions unique notNull primaryKey)
  | primaryKey = "primary key"
  | otherwise = [sql|#{u} #{nn}|]
  where
    u | unique = "unique"
      | otherwise = ""
    nn | notNull = "not null"
       | otherwise = ""
