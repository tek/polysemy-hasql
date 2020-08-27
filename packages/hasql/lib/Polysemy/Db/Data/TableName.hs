module Polysemy.Db.Data.TableName where

newtype TableName =
  TableName Text
  deriving (Eq, Show)
  deriving newtype (IsString)
