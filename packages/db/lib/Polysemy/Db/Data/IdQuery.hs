module Polysemy.Db.Data.IdQuery where

import Polysemy.Db.Data.Column (Auto, Prim)

data IdQuery i =
  IdQuery { id :: i }
  deriving (Eq, Show, Generic)

data IdQueryRep =
  IdQueryRep { id :: Prim Auto }
  deriving (Eq, Show, Generic)

type UuidQuery =
  IdQuery UUID
