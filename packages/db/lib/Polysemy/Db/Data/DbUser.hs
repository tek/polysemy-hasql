module Polysemy.Db.Data.DbUser where

newtype DbUser =
  DbUser Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

json ''DbUser
