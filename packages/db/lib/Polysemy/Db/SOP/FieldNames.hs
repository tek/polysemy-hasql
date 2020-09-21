module Polysemy.Db.SOP.FieldNames where

import Generics.SOP.GGP (GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record), DatatypeInfo(ADT), FieldInfo(FieldInfo))

type family CtorFieldSymbols (fs :: [FieldInfo]) :: [Symbol] where
  CtorFieldSymbols '[] = '[]
  CtorFieldSymbols ('FieldInfo name : fs) = name : CtorFieldSymbols fs

type family CtorsFieldSymbols (fss :: [[FieldInfo]]) :: [[Symbol]] where
  CtorsFieldSymbols '[] = '[]
  CtorsFieldSymbols (fs : fss) = CtorFieldSymbols fs : CtorsFieldSymbols fss

type family CtorsFields (cs :: [ConstructorInfo]) :: [[FieldInfo]] where
  CtorsFields '[] = '[]
  CtorsFields ('Record _ fields : cs) = fields : CtorsFields cs

type family ADTCtorsFields dt :: [[FieldInfo]] where
  ADTCtorsFields ('ADT _ _ ctors _) = CtorsFields ctors

class DemoteFieldNames (d :: *) where
  type FieldNames d :: [[Symbol]]

instance DemoteFieldNames d where
  type FieldNames d = CtorsFieldSymbols (ADTCtorsFields (GDatatypeInfoOf d))
