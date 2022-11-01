module Sqel.Sql.Delete where

newtype Delete a =
  Delete { unDelete :: a }
  deriving stock (Eq, Show)
