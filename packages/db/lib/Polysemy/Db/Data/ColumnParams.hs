module Polysemy.Db.Data.ColumnParams where

data ColumnParams =
  ColumnParams {
     unique :: Bool,
     notNull :: Bool,
     primaryKey :: Bool
  }
  deriving (Eq, Show)

instance Default ColumnParams where
  def =
    ColumnParams False True False

instance Semigroup ColumnParams where
  ColumnParams u1 n1 p1 <> ColumnParams u2 n2 p2 =
    ColumnParams (u1 || u2) (n1 && n2) (p1 || p2)
