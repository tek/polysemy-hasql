{-# options_ghc -Wno-redundant-constraints #-}
{-# language CPP #-}
#define sop5 MIN_VERSION_generics_sop(0,5,0)

module Polysemy.Hasql.SOP.Enum where

import qualified Data.Map.Strict as Map
import qualified Generics.SOP as SOP
import Generics.SOP (
  I,
  K(K),
  NP(Nil),
  NS,
  SOP(SOP),
  constructorName,
  hcollapse,
  hczipWith,
  injections,
  type (-.->),
  unK,
  )
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, gto)
import qualified Generics.SOP.Type.Metadata as T

import Sqel.SOP.Constraint (IsEnum, IsNullary, ReifySOP)

#if sop5
genEnumTable ::
  ∀ a mod name ctors strictness .
  IsEnum a =>
  ReifySOP a (GCode a) =>
  GDatatypeInfoOf a ~ 'T.ADT mod name ctors strictness =>
  T.DemoteConstructorInfos ctors (GCode a) =>
  Map Text a

#else
genEnumTable ::
  ∀ a mod name ctors .
  IsEnum a =>
  ReifySOP a (GCode a) =>
  GDatatypeInfoOf a ~ 'T.ADT mod name ctors =>
  T.DemoteConstructorInfos ctors (GCode a) =>
  Map Text a

#endif

genEnumTable =
  Map.fromList (hcollapse cs)
  where
    cs =
      hczipWith (Proxy :: Proxy IsNullary) f ctors injections
    f :: SOP.ConstructorInfo fs -> (NP I -.-> K (NS (NP I) (GCode a))) '[] -> K (Text, a) '[]
    f ctor (SOP.Fn inject) =
      K (toText (constructorName ctor), gto (SOP (unK (inject Nil))))
    ctors =
      T.demoteConstructorInfos (Proxy @ctors)

class EnumTable a where
  enumTable :: Map Text a

instance (
    IsEnum a,
    ReifySOP a (GCode a),
    GDatatypeInfoOf a ~ 'T.ADT mod name ctors strictness,
    T.DemoteConstructorInfos ctors (GCode a)
  ) => EnumTable a where
    enumTable =
      genEnumTable
