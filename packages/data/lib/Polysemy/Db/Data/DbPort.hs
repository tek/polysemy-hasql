{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbPort where

import Data.Default (Default(def))
import Prelude (Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show)

import Polysemy.Db.Json (defaultJson)

newtype DbPort =
  DbPort Int
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read)

instance Default DbPort where
  def =
    5432

defaultJson ''DbPort
