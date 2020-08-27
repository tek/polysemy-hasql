module Polysemy.Db.SOP.Constraint where

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Generics.SOP (All, DatatypeInfoOf, IsProductType)
import Generics.SOP.Type.Metadata (
  ConstructorInfo(Record),
  DatatypeInfo(ADT, Newtype),
  DemoteFieldInfos,
  FieldInfo,
  )
import Prelude hiding (All, Generic)

import Polysemy.Db.Text.Case (unCamelCase)

type IsNullary =
  (~) '[]

type AllProduct (c :: * -> Constraint) a (xs :: [*]) =
  (IsProductType a xs, All c xs)

type SymbolNames names =
  All KnownSymbol names

type FieldNames fields names =
  DemoteFieldInfos fields names

class IsDataT (d :: DatatypeInfo) (name :: Symbol) | d -> name where

instance IsDataT ('Newtype mod name ctors) name where

instance IsDataT ('ADT mod name ctors strictness) name where

class IsData a types name | a -> types name where

instance
  (
  IsProductType a types,
  IsDataT (DatatypeInfoOf a) name
  ) =>
    IsData a types name where

class IsRecordT (d :: DatatypeInfo) (name :: Symbol) names | d -> name names where

instance IsRecordT ('Newtype mod name ('Record ctor names)) name names where

instance IsRecordT ('ADT mod name '[ 'Record ctor names] strictness) name names where

class IsRecord a types name names | a -> types name names where

instance
  (
  IsProductType a types,
  IsRecordT (DatatypeInfoOf a) name names
  ) =>
    IsRecord a types name names where

class DataName a where
  dataName :: String

instance
  ∀ a name .
  (
  KnownSymbol name,
  IsDataT (DatatypeInfoOf a) name
  ) =>
  DataName a where
    dataName =
      symbolVal (Proxy @name)

dataSlug ::
  ∀ a .
  DataName a =>
  Text
dataSlug =
  unCamelCase '-' (dataName @a)

dataSlug_ ::
  ∀ a .
  DataName a =>
  Text
dataSlug_ =
  unCamelCase '_' (dataName @a)

class RecordFields a (names :: [FieldInfo]) types | a -> names types where

instance
  ∀ a types name names .
  IsRecord a types name names =>
  RecordFields a names types where
