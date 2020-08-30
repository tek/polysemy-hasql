module Polysemy.Db.Data.Column where

import Polysemy.Db.Data.Uid (Uid(Uid))

data PrimaryKey =
  PrimaryKey
  deriving (Eq, Show)

data ForeignKey =
  ForeignKey
  deriving (Eq, Show)

data Flatten r =
  Flatten
  deriving (Eq, Show)

data Auto =
  Auto
  deriving (Eq, Show)

data Unique =
  Unique
  deriving (Eq, Show)

data Prim a =
  Prim
  deriving (Eq, Show)

-- this could be used as a functorial parameter in logic.
-- @f Archive@, with the store being @Store f e Archive@ or @Store i f e Archive@
data PK i a =
  PK {
    id :: i,
    payload :: a
  }
  deriving (Eq, Show)

deriveGeneric ''PK

uidToPK :: Uid i a -> PK i a
uidToPK (Uid id' a) =
  PK id' a

pkToUid :: PK i a -> Uid i a
pkToUid (PK id' a) =
  Uid id' a

newtype PKQuery i =
  PKQuery {
    id :: i
  }
  deriving (Eq, Show)

deriveGeneric ''PKQuery

data PKRep i r =
  PKRep {
    id :: Prim PrimaryKey,
    payload :: Flatten r
  }
  deriving (Eq, Show)

deriveGeneric ''PKRep
