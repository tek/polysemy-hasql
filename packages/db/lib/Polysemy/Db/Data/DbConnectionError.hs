module Polysemy.Db.Data.DbConnectionError where

data DbConnectionError =
  Acquire Text
  |
  Release Text
  |
  Query Text
  deriving stock (Eq, Show, Generic)
