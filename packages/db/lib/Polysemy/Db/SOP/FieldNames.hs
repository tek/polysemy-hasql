{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.SOP.FieldNames where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo (Constructor, Record), DatatypeInfo (ADT), FieldInfo (FieldInfo))

import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))

type family RecordFieldSymbols (fs :: [FieldInfo]) :: [FieldId] where
  RecordFieldSymbols '[] = '[]
  RecordFieldSymbols ('FieldInfo name : fs) = 'NamedField name : RecordFieldSymbols fs

type family CtorFieldSymbols (con :: Symbol) (index :: Nat) (ds :: [Type]) :: [FieldId] where
  CtorFieldSymbols _ _ '[] = '[]
  CtorFieldSymbols con index (_ : ds) =
    'NumberedField con index : CtorFieldSymbols con (index + 1) ds

type family CtorsFields (cs :: [ConstructorInfo]) (dss :: [[Type]]) :: [[FieldId]] where
  CtorsFields '[] '[] =
    '[]
  CtorsFields ('Record _ fields : cs) (_ : dss) =
    RecordFieldSymbols fields : CtorsFields cs dss
  CtorsFields ('Constructor name : cs) (ds : dss) =
    CtorFieldSymbols name 1 ds : CtorsFields cs dss

type family ADTCtorsFields (adt :: DatatypeInfo) (dss :: [[Type]]) :: [[FieldId]] where
#if sop5
  ADTCtorsFields ('ADT _ _ ctors _) dss =
    CtorsFields ctors dss

#else
  ADTCtorsFields ('ADT _ _ ctors) dss =
    CtorsFields ctors dss

#endif

class DemoteFieldNames (d :: Type) where
  type FieldIds d :: [[FieldId]]

instance DemoteFieldNames d where
  type FieldIds d = ADTCtorsFields (GDatatypeInfoOf d) (GCode d)

data FieldInfoSymbol :: FieldInfo -> Exp Symbol
type instance Eval (FieldInfoSymbol ('FieldInfo name)) = name

type family FieldNames' (info :: DatatypeInfo) :: [Symbol] where
  FieldNames' ('ADT _ _ '[ 'Record _ fs] _) = FMap FieldInfoSymbol @@ fs

type family FieldNames d :: [Symbol] where
  FieldNames d = FieldNames' (GDatatypeInfoOf d)
