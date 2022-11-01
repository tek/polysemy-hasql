module Sqel.Sql.Returning where

newtype Returning a =
  Returning { unReturning :: a }
  deriving stock (Eq, Show)
