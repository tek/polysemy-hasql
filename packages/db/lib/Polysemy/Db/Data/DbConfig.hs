module Polysemy.Db.Data.DbConfig where

import Polysemy.Db.Data.DbHost (DbHost)
import Polysemy.Db.Data.DbName (DbName)
import Polysemy.Db.Data.DbPassword (DbPassword)
import Polysemy.Db.Data.DbPort (DbPort)
import Polysemy.Db.Data.DbUser (DbUser)

-- |Connection information for a database.
-- >>> DbConfig "localhost" 5432 "users" "post" "gres"
data DbConfig =
  DbConfig {
    host :: DbHost,
    port :: DbPort,
    name :: DbName,
    user :: DbUser,
    password :: DbPassword
  }
  deriving stock (Eq, Show, Generic)

json ''DbConfig
