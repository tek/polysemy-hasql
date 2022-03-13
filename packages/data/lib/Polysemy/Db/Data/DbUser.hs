{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbUser where

import Polysemy.Db.Json (defaultJson)
import Prelude (Text, Eq, Show, IsString)

newtype DbUser =
  DbUser Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbUser
