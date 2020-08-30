module Polysemy.Hasql.Data.SqlCode where

data SqlCode =
  SqlCode { unSqlCode :: Text }
  deriving (Eq, Show)
