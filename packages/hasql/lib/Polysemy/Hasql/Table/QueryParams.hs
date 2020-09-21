module Polysemy.Hasql.Table.QueryParams where

import Data.Functor.Contravariant.Divisible (choose)
import Generics.SOP (
  All,
  I,
  K(K),
  NP(Nil, (:*)),
  NS,
  NS(Z, S),
  Projection,
  Top,
  hcollapse,
  hindex,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unI,
  unSOP,
  unZ,
  )
import Generics.SOP.GGP (GCode, gfrom)
import Hasql.Encoders (Params)
import Prelude hiding (All, Enum)

import Polysemy.Db.Data.Column (Enum, Flatten, Prim, Sum)
import Polysemy.Db.SOP.Constraint (ConstructSOP)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNP)
import Polysemy.Hasql.Table.QueryParam (NullVariant, NullVariants, QueryParam(queryParam), writeNulls, writeNulls2)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)

data Done =
  Done
  deriving (Show)

data Single (tail :: [*]) =
  Single
  deriving (Show)

data Multi (head :: *) (tail :: [*]) =
  Multi
  deriving (Show)

type family UnconsRep (reps :: [*]) :: *
type instance UnconsRep '[] = Done
type instance UnconsRep (Sum r : reps) = Multi r reps
type instance UnconsRep (Flatten r : reps) = Multi r reps
type instance UnconsRep (Prim r : reps) = Single reps
type instance UnconsRep (Enum r : reps) = Single reps

class GenParams (reps :: *) (ds :: [*]) where
  genParams :: NP Params ds

instance GenParams Done '[] where
  genParams =
    Nil

instance (
    QueryParam d,
    GenParams (UnconsRep reps) ds
  ) => GenParams (Single reps) (d : ds) where
    genParams =
      queryParam @d :* genParams @(UnconsRep reps) @ds

instance (
    GenQueryParams head d (GCode d),
    GenParams (UnconsRep tail) ds
  ) => GenParams (Multi head tail) (d : ds) where
    genParams =
      genQueryParams @head @d @(GCode d) :* genParams @(UnconsRep tail) @ds

queryParamsNP ::
  ∀ (reps :: *) ds d .
  GenParams reps ds =>
  ConstructSOP d '[ds] =>
  Params d
queryParamsNP =
  contramap unpackSOP res
  where
    res =
      mconcat clps
    clps =
      hcollapse qps
    unpackSOP =
      unZ . unSOP . gfrom
    qps :: NP (K (Params (NP I ds))) ds
    qps =
      hzipWith qp (genParams @reps @ds :: NP Params ds) (projections :: NP (Projection I ds) ds)
    qp :: ∀ a . Params a -> Projection I ds a -> K (Params (NP I ds)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

class SumParams (repss :: [*]) (dss :: [[*]]) where
  sumParams :: Params (NS (NP I) dss)

instance SumParams '[] '[] where
  sumParams =
    mempty

instance (
    All Top ds,
    NullVariant reps ds,
    NullVariants repss dss,
    SumParams repss dss,
    GenParams (UnconsRep reps) ds
  ) => SumParams (ProdColumn reps : repss) (ds : dss) where
  sumParams =
    choose unconsNS inhabited uninhabited
    where
      inhabited =
        sequenceContravariantNP (genParams @(UnconsRep reps)) <> writeNulls2 @repss @dss
      uninhabited =
        writeNulls @reps @ds <> sumParams @repss

sumIndex ::
  Params (NS (NP I) dss)
sumIndex =
  contramap hindex queryParam

queryParamsNS ::
  ∀ (d :: *) (repss :: [*]) (dss :: [[*]]) .
  ConstructSOP d dss =>
  SumParams repss dss =>
  Params d
queryParamsNS =
  contramap (unSOP . gfrom) (sumIndex <> sumParams @repss)

class GenQueryParams (rep :: *) (d :: *) (dss :: [[*]]) where
  genQueryParams :: Params d

instance (
    ConstructSOP d '[ds],
    GenParams (UnconsRep reps) ds
  ) => GenQueryParams (ProdColumn reps) d '[ds] where
  genQueryParams =
    queryParamsNP @(UnconsRep reps) @ds

instance (
    ConstructSOP d dss,
    SumParams reps dss
  ) => GenQueryParams (SumColumn reps) d dss where
    genQueryParams =
      queryParamsNS @d @reps @dss

class QueryParams (rep :: *) (d :: *) where
  queryParams :: Params d

instance GenQueryParams (ReifyRepTable rep d) d (GCode d) => QueryParams rep d where
    queryParams =
      genQueryParams @(ReifyRepTable rep d) @d @(GCode d)
