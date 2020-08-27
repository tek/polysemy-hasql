module Polysemy.Db.Accounts where

import Polysemy.Db.Data.Column (Auto, Prim)

data AccountRep =
  AccountRep {
     name :: Prim Auto,
     roles :: Prim Auto,
     status :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''AccountRep

data AccountAuthRep =
  AccountAuthRep {
    account :: Prim Auto,
    description :: Prim Auto,
    password :: Prim Auto,
    permissions :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''AccountAuthRep
