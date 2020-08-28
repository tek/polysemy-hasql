module Polysemy.Db.Data.DbError where

data DbError =
  Connection Text
  |
  Query Text
  |
  Table Text
  deriving (Eq, Show)
