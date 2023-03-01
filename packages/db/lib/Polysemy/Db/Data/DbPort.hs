module Polysemy.Db.Data.DbPort where

newtype DbPort =
  DbPort { unDbPort :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Read, Num, Real, Enum, Integral, Ord)

instance Default DbPort where
  def = 5432

json ''DbPort
