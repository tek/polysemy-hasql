{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Db.SOP.Constructor where

import Generics.SOP.GGP (GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record, Constructor), DatatypeInfo(ADT))

type family CtorNames (ctors :: [ConstructorInfo]) :: [Symbol] where
  CtorNames '[] =
    '[]
  CtorNames ('Constructor name : ctors) =
    name : CtorNames ctors
  CtorNames ('Record name _ : ctors) =
    name : CtorNames ctors

type family ADTCtorNames dt :: [Symbol] where
#if sop5
  ADTCtorNames ('ADT _ _ ctors _) = CtorNames ctors
#else
  ADTCtorNames ('ADT _ _ ctors) = CtorNames ctors
#endif

class DemoteConstructorNames (d :: *) where
  type ConstructorNames d :: [Symbol]

instance DemoteConstructorNames d where
  type ConstructorNames d =
    ADTCtorNames (GDatatypeInfoOf d)
