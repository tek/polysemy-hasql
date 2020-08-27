 {-# OPTIONS_GHC -fclear-plugins #-}

module Polysemy.Db.Data.DbConfig where

import Polysemy.Db.Data.DbHost (DbHost)
import Polysemy.Db.Data.DbName (DbName)
import Polysemy.Db.Data.DbPassword (DbPassword)
import Polysemy.Db.Data.DbPort (DbPort)
import Polysemy.Db.Data.DbUser (DbUser)

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
