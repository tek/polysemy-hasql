{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbHost where

import Data.Default (Default(def))
import Prelude (Eq, IsString, Show, Text)

import Polysemy.Db.Json (defaultJson)

newtype DbHost =
  DbHost Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance Default DbHost where
  def =
    "localhost"

defaultJson ''DbHost
