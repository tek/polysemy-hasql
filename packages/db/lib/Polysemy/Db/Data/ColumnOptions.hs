module Polysemy.Db.Data.ColumnOptions where

data ColumnOptions =
  ColumnOptions {
     unique :: Bool,
     notNull :: Bool,
     primaryKey :: Bool
  }
  deriving (Eq, Show, Ord)

instance Default ColumnOptions where
  def =
    ColumnOptions False True False

instance Semigroup ColumnOptions where
  ColumnOptions u1 n1 p1 <> ColumnOptions u2 n2 p2 =
    ColumnOptions (u1 || u2) (n1 && n2) (p1 || p2)
