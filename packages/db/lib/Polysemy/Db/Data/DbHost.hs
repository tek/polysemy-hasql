module Polysemy.Db.Data.DbHost where

newtype DbHost =
  DbHost { unDbHost :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

instance Default DbHost where
  def = "localhost"

json ''DbHost
