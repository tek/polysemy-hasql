module Sqel.Sql.Insert where

newtype Insert a =
  Insert { unInsert :: a }
  deriving stock (Eq, Show)
