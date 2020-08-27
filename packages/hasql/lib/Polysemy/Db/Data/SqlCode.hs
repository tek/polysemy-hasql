module Polysemy.Db.Data.SqlCode where

data SqlCode =
  SqlCode { unSqlCode :: Text }
  deriving (Eq, Show)
