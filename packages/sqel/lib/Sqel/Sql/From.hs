module Sqel.Sql.From where

newtype From a =
  From { unFrom :: a }
  deriving stock (Eq, Show, Generic)
