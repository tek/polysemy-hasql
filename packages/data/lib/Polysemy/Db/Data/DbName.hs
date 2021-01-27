{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbName where

import Prelude (Eq, IsString, Show, Text)

import Polysemy.Db.Json (defaultJson)

newtype DbName =
  DbName Text
  deriving (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbName
