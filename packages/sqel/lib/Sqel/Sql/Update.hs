module Sqel.Sql.Update where

newtype Update a =
  Update { unUpdate :: a }
  deriving stock (Eq, Show, Generic)
