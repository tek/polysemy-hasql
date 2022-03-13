module Polysemy.Db.Data.Rep where

data Rep (r :: [Type]) =
  Rep
  deriving stock (Eq, Show)

data ForceRep (r :: [Type]) =
  ForceRep
  deriving stock (Eq, Show)

type family ConsRep (h :: Type) (t :: Type) :: Type where
  ConsRep (Rep h) (Rep t) = (Rep (h ++ t))
  ConsRep h (Rep t) = Rep (h : t)
  ConsRep h t = Rep [h, t]

data PrimaryKey =
  PrimaryKey
  deriving stock (Eq, Show)

data ForeignKey =
  ForeignKey
  deriving stock (Eq, Show)

data Flatten adt =
  Flatten
  deriving stock (Eq, Show)

data Product adt =
  Product
  deriving stock (Eq, Show)

data Auto =
  Auto
  deriving stock (Eq, Show)

data Unique =
  Unique
  deriving stock (Eq, Show)

data Prim =
  Prim
  deriving stock (Eq, Show)

data ForcePrim a =
  ForcePrim
  deriving stock (Eq, Show)

data Sum adt =
  Sum
  deriving stock (Eq, Show)

data Enum =
  Enum
  deriving stock (Eq, Show)

data Json =
  Json
  deriving stock (Eq, Show)

data JsonB =
  JsonB
  deriving stock (Eq, Show)

data PrimQuery (field :: Symbol) =
  PrimQuery
  deriving stock (Eq, Show)

type IdQuery =
  PrimQuery "id"

data PrimQueryAs (field :: Symbol) (rep :: Type) =
  IdQueryAs
  deriving stock (Eq, Show)

type IdQueryAs (rep :: Type) =
  PrimQueryAs "id" rep

data UidRep i a =
  UidRep {
     id :: i,
     payload :: Flatten a
  }
  deriving stock (Generic)

data UidNestRep i a =
  UidNestRep {
     id :: i,
     payload :: a
  }
  deriving stock (Generic)

type UuidRep a =
  UidRep Auto a
