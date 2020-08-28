 {-# OPTIONS_GHC -fclear-plugins #-}

module Polysemy.Db.Data.DbPort where

newtype DbPort =
  DbPort Int
  deriving (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read)

instance Default DbPort where
  def =
    9122

defaultJson ''DbPort
