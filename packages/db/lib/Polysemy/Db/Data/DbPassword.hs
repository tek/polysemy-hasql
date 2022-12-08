module Polysemy.Db.Data.DbPassword where

newtype DbPassword =
  DbPassword { unDbPassword :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''DbPassword
