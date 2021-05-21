module Polysemy.Db.Data.Column where

data Rep (r :: [*]) =
  Rep
  deriving (Eq, Show)

data ForceRep (r :: [*]) =
  ForceRep
  deriving (Eq, Show)

type family ConsRep (h :: *) (t :: *) :: * where
  ConsRep (Rep h) (Rep t) = (Rep (h ++ t))
  ConsRep h (Rep t) = Rep (h : t)
  ConsRep h t = Rep [h, t]

data PrimaryKey =
  PrimaryKey
  deriving (Eq, Show)

data ForeignKey =
  ForeignKey
  deriving (Eq, Show)

data Flatten adt =
  Flatten
  deriving (Eq, Show)

data Product adt =
  Product
  deriving (Eq, Show)

data Auto =
  Auto
  deriving (Eq, Show)

data Unique =
  Unique
  deriving (Eq, Show)

data Prim =
  Prim
  deriving (Eq, Show)

data ForcePrim a =
  ForcePrim
  deriving (Eq, Show)

data Sum adt =
  Sum
  deriving (Eq, Show)

data Enum =
  Enum
  deriving (Eq, Show)

data Json =
  Json
  deriving (Eq, Show)

data JsonB =
  JsonB
  deriving (Eq, Show)

data PrimQuery (field :: Symbol) =
  PrimQuery
  deriving (Eq, Show)

data UidRep i a =
  UidRep {
     id :: i,
     payload :: Flatten a
  }
  deriving (Generic)

data UidNestRep i a =
  UidNestRep {
     id :: i,
     payload :: a
  }
  deriving (Generic)

type UuidRep a =
  UidRep Auto a
