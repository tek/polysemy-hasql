{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.SOP.Constraint where

import GHC.TypeLits (symbolVal)
import Generics.SOP (All, All2, Top)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, GFrom, GTo)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record), DatatypeInfo(ADT, Newtype), FieldInfo)

import Polysemy.Db.Text.Case (unCamelCase, unCamelCaseString)

type Coded (d :: *) (dss :: [[*]]) =
  GCode d ~ dss

type ProductCoded (d :: *) (ds :: [*]) =
  Coded d '[ds]

type NewtypeCoded (d :: *) (a :: *) =
  (Coercible d a, ProductCoded d '[a])

type ReifySOP (d :: *) (dss :: [[*]]) =
  (Generic d, GTo d, GCode d ~ dss, All2 Top dss)

type ConstructSOP (d :: *) (dss :: [[*]]) =
  (Generic d, GFrom d, GCode d ~ dss, All2 Top dss)

type IsNullary =
  (~) '[]

type IsEnum a =
  All IsNullary (GCode a)

class IsDataT (d :: DatatypeInfo) (name :: Symbol) | d -> name where

instance IsDataT ('Newtype mod name ctors) name where

#if sop5
instance IsDataT ('ADT mod name ctors strictness) name where
#else
instance IsDataT ('ADT mod name ctors) name where
#endif

class IsData (a :: *) (types :: [*]) (name :: Symbol) | a -> types name where

instance (
    ProductCoded a types,
    IsDataT (GDatatypeInfoOf a) name
  ) =>
    IsData a types name where

class IsRecordT (d :: DatatypeInfo) (name :: Symbol) names | d -> name names where

instance IsRecordT ('Newtype mod name ('Record ctor names)) name names where

#if sop5
instance IsRecordT ('ADT mod name '[ 'Record ctor names] strictness) name names where

#else
instance IsRecordT ('ADT mod name '[ 'Record ctor names]) name names where

#endif

class IsRecord (a :: *) (types :: [*]) (name :: Symbol) (fields :: [FieldInfo]) | a -> types name fields where

instance (
    ProductCoded a types,
    IsRecordT (GDatatypeInfoOf a) name fields
  ) =>
    IsRecord a types name fields where

class CtorsT (d :: DatatypeInfo) (ctors :: [ConstructorInfo]) | d -> ctors where

#if sop5
instance CtorsT ('ADT mod name ctors strictness) ctors where

#else
instance CtorsT ('ADT mod name ctors) ctors where

#endif

class Ctors (d :: *) (ctors :: [ConstructorInfo]) (types :: [[*]]) | d -> ctors types where

instance (CtorsT (GDatatypeInfoOf d) ctors, types ~ GCode d) => Ctors d ctors types where

class DataName (d :: *) (name :: Symbol) | d -> name where
  dataNameString :: String

instance (
  KnownSymbol name,
  IsDataT (GDatatypeInfoOf d) name
  ) => DataName d name where
    dataNameString =
      symbolVal (Proxy @name)

dataName ::
  ∀ d name .
  DataName d name =>
  Text
dataName =
  toText (dataNameString @d)

symbolString ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  String
symbolString =
  symbolVal (Proxy @name)

symbolText ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  Text
symbolText =
  toText (symbolString @name)

dataSlug ::
  ∀ a name .
  DataName a name =>
  Text
dataSlug =
  unCamelCase '-' (dataNameString @a)

slugString_ ::
  String ->
  String
slugString_ =
  unCamelCaseString '_'

slugText_ ::
  String ->
  Text
slugText_ =
  unCamelCase '_'

slugSymbolString_ ::
  ∀ a .
  KnownSymbol a =>
  String
slugSymbolString_ =
  slugString_ (symbolVal (Proxy @a))

slugSymbol_ ::
  ∀ a .
  KnownSymbol a =>
  Text
slugSymbol_ =
  slugText_ (symbolVal (Proxy @a))

dataSlug_ ::
  ∀ a name .
  DataName a name =>
  Text
dataSlug_ =
  slugText_ (dataNameString @a)

class RecordFields a (names :: [FieldInfo]) types | a -> names types where

instance
  ∀ a types name names .
  IsRecord a types name names =>
  RecordFields a names types where
