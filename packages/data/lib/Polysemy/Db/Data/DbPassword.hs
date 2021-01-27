{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbPassword where

import Polysemy.Db.Json (defaultJson)
import Prelude (Eq, IsString, Show, Text)

newtype DbPassword =
  DbPassword Text
  deriving (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbPassword
