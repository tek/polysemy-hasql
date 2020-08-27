{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Polysemy.Db.SOP.Enum where

import qualified Data.Map.Strict as Map
import Generics.SOP (
  Code,
  DatatypeInfoOf,
  I,
  IsEnumType,
  K(K),
  NP(Nil),
  NS,
  SOP(SOP),
  constructorName,
  hcollapse,
  hczipWith,
  injections,
  to,
  type (-.->),
  unK,
  )
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Type.Metadata as T

genEnumTable ::
  ∀ a mod name ctors strictness .
  IsEnumType a =>
  DatatypeInfoOf a ~ 'T.ADT mod name ctors strictness =>
  T.DemoteConstructorInfos ctors (Code a) =>
  Map Text a
genEnumTable =
  Map.fromList (hcollapse cs)
  where
    cs =
      hczipWith (Proxy :: Proxy ((~) '[])) f ctors injections
    f :: SOP.ConstructorInfo fs -> (NP I -.-> K (NS (NP I) (Code a))) '[] -> K (Text, a) '[]
    f ctor (SOP.Fn inject) =
      K (toText (constructorName ctor), to (SOP (unK (inject Nil))))
    ctors =
      T.demoteConstructorInfos (Proxy @ctors)

class EnumTable a where
  enumTable :: Map Text a

instance
  (
  IsEnumType a,
  DatatypeInfoOf a ~ 'T.ADT mod name ctors strictness,
  T.DemoteConstructorInfos ctors (Code a)
  ) =>
  EnumTable a where
  enumTable =
    genEnumTable
