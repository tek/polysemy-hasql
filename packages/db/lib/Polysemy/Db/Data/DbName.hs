module Polysemy.Db.Data.DbName where

newtype DbName =
  DbName { unDbName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)

json ''DbName
