{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.DbConfig where

import Control.Lens (makeClassy)
import Prelude (Eq, Generic, Show)

import Polysemy.Db.Data.DbHost (DbHost)
import Polysemy.Db.Data.DbName (DbName)
import Polysemy.Db.Data.DbPassword (DbPassword)
import Polysemy.Db.Data.DbPort (DbPort)
import Polysemy.Db.Data.DbUser (DbUser)
import Polysemy.Db.Json (defaultJson)

data DbConfig =
  DbConfig {
    _host :: DbHost,
    _port :: DbPort,
    _name :: DbName,
    _user :: DbUser,
    _password :: DbPassword
  }
  deriving (Eq, Show, Generic)

makeClassy ''DbConfig
defaultJson ''DbConfig
