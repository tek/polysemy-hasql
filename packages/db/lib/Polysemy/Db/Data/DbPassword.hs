module Polysemy.Db.Data.DbPassword where

newtype DbPassword =
  DbPassword Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

json ''DbPassword
