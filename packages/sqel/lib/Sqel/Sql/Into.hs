module Sqel.Sql.Into where

newtype Into a =
  Into { unInto :: a }
  deriving stock (Eq, Show, Generic)
