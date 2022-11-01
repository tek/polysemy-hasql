module Sqel.Sql.SelectQuery where

newtype SelectQuery a =
  SelectQuery { unSelectQuery :: a }
  deriving stock (Eq, Show, Generic)
