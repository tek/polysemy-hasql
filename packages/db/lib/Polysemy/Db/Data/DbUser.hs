module Polysemy.Db.Data.DbUser where

newtype DbUser =
  DbUser { unDbUser :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''DbUser
