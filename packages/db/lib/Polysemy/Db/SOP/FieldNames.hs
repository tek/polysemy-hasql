module Polysemy.Db.SOP.FieldNames where

import Fcf (Eval)
import GHC.TypeLits (type (+))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Constructor, Record), DatatypeInfo(ADT), FieldInfo(FieldInfo))

import Polysemy.Db.Data.FieldId (FieldId(NumberedField, NamedField))
import Polysemy.Db.SOP.Text (CanonicalName)

type family RecordFieldSymbols (fs :: [FieldInfo]) :: [FieldId] where
  RecordFieldSymbols '[] = '[]
  RecordFieldSymbols ('FieldInfo name : fs) = 'NamedField (Eval (CanonicalName name)) : RecordFieldSymbols fs

type family CtorFieldSymbols (con :: Symbol) (index :: Nat) (ds :: [*]) :: [FieldId] where
  CtorFieldSymbols _ _ '[] = '[]
  CtorFieldSymbols con index (_ : ds) =
    'NumberedField con index : CtorFieldSymbols con (index + 1) ds

type family CtorsFields (cs :: [ConstructorInfo]) (dss :: [[*]]) :: [[FieldId]] where
  CtorsFields '[] '[] =
    '[]
  CtorsFields ('Record _ fields : cs) (ds : dss) =
    RecordFieldSymbols fields : CtorsFields cs dss
  CtorsFields ('Constructor name : cs) (ds : dss) =
    CtorFieldSymbols name 1 ds : CtorsFields cs dss

type family ADTCtorsFields (adt :: DatatypeInfo) (dss :: [[*]]) :: [[FieldId]] where
  ADTCtorsFields ('ADT _ _ ctors _) dss =
    CtorsFields ctors dss

class DemoteFieldNames (d :: *) where
  type FieldNames d :: [[FieldId]]

instance DemoteFieldNames d where
  type FieldNames d = ADTCtorsFields (GDatatypeInfoOf d) (GCode d)
