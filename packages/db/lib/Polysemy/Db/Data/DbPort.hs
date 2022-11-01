module Polysemy.Db.Data.DbPort where

newtype DbPort =
  DbPort Int
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read)

instance Default DbPort where
  def =
    5432

json ''DbPort
