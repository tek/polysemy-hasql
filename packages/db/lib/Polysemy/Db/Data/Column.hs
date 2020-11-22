module Polysemy.Db.Data.Column where

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

data Sum r s =
  Sum
  deriving (Eq, Show)

data Enum a =
  Enum
  deriving (Eq, Show)

data UidRep i a =
  UidRep {
     id :: i,
     payload :: Flatten a
  }
  deriving (Eq, Show, Generic)
