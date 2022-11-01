module Polysemy.Db.Data.DbHost where

newtype DbHost =
  DbHost Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance Default DbHost where
  def =
    "localhost"

json ''DbHost
