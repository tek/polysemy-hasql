{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.SOP.FieldNames where

import GHC.TypeLits (type (+))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Constructor, Record), DatatypeInfo(ADT), FieldInfo(FieldInfo))

import Polysemy.Db.Data.FieldId (FieldId(NumberedField, NamedField))
import Fcf (type (@@), Eval, Exp)
import Fcf.Class.Functor (FMap)

type family RecordFieldSymbols (fs :: [FieldInfo]) :: [FieldId] where
  RecordFieldSymbols '[] = '[]
  RecordFieldSymbols ('FieldInfo name : fs) = 'NamedField name : RecordFieldSymbols fs

type family CtorFieldSymbols (con :: Symbol) (index :: Nat) (ds :: [*]) :: [FieldId] where
  CtorFieldSymbols _ _ '[] = '[]
  CtorFieldSymbols con index (_ : ds) =
    'NumberedField con index : CtorFieldSymbols con (index + 1) ds

type family CtorsFields (cs :: [ConstructorInfo]) (dss :: [[*]]) :: [[FieldId]] where
  CtorsFields '[] '[] =
    '[]
  CtorsFields ('Record _ fields : cs) (_ : dss) =
    RecordFieldSymbols fields : CtorsFields cs dss
  CtorsFields ('Constructor name : cs) (ds : dss) =
    CtorFieldSymbols name 1 ds : CtorsFields cs dss

type family ADTCtorsFields (adt :: DatatypeInfo) (dss :: [[*]]) :: [[FieldId]] where
#if sop5
  ADTCtorsFields ('ADT _ _ ctors _) dss =
    CtorsFields ctors dss

#else
  ADTCtorsFields ('ADT _ _ ctors) dss =
    CtorsFields ctors dss

#endif

class DemoteFieldNames (d :: *) where
  type FieldNames d :: [[FieldId]]

instance DemoteFieldNames d where
  type FieldNames d = ADTCtorsFields (GDatatypeInfoOf d) (GCode d)

data FieldInfoSymbol :: FieldInfo -> Exp Symbol
type instance Eval (FieldInfoSymbol ('FieldInfo name)) = name

type family SimpleFieldNames' (info :: DatatypeInfo) :: [Symbol] where
  SimpleFieldNames' ('ADT _ _ '[ 'Record _ fs] _) = FMap FieldInfoSymbol @@ fs

type family SimpleFieldNames d :: [Symbol] where
  SimpleFieldNames d = SimpleFieldNames' (GDatatypeInfoOf d)
