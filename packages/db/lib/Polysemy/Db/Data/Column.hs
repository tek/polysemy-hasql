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

data NewtypePrim a =
  NewtypePrim
  deriving (Eq, Show)

data Sum a =
  Sum
  deriving (Eq, Show)

data Enum a =
  Enum
  deriving (Eq, Show)

data PK (f :: * -> *) i a =
  PK {
    id :: i,
    payload :: a
  }
  deriving (Eq, Show, Generic)

uidToPK :: Uid i a -> PK f i a
uidToPK (Uid id' a) =
  PK id' a

pkToUid :: PK f i a -> Uid i a
pkToUid (PK id' a) =
  Uid id' a

newtype PKQuery i =
  PKQuery {
    id :: i
  }
  deriving (Eq, Show, Generic)

data PKRep f i r =
  PKRep {
    id :: f PrimaryKey,
    payload :: Flatten r
  }
  deriving (Generic)

deriving instance Show (PKRep Prim i r)
deriving instance Show (PKRep NewtypePrim i r)
