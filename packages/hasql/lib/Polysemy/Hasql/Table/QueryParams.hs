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

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.SOP.Constraint (ConstructSOP)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNP)
import Polysemy.Hasql.Table.ColumnType (Done, Multi, Single, UnconsRep)
import Polysemy.Hasql.Table.QueryParam (NullVariant, NullVariants, QueryParam(queryParam), writeNulls, writeNulls2)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)

class ProductParams (reps :: *) (ds :: [*]) where
  productParams :: NP Params ds

instance ProductParams Done '[] where
  productParams =
    Nil

instance (
    QueryParam rep d,
    ProductParams (UnconsRep reps) ds
  ) => ProductParams (Single rep reps) (d : ds) where
    productParams =
      queryParam @rep @d :* productParams @(UnconsRep reps) @ds

instance (
    ColumnParams head d (GCode d),
    ProductParams (UnconsRep tail) ds
  ) => ProductParams (Multi head tail) (d : ds) where
    productParams =
      columnParams @head @d @(GCode d) :* productParams @(UnconsRep tail) @ds

queryParamsNP ::
  ∀ (reps :: *) ds d .
  ProductParams reps ds =>
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
      hzipWith qp (productParams @reps @ds :: NP Params ds) (projections :: NP (Projection I ds) ds)
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
    ProductParams (UnconsRep reps) ds
  ) => SumParams (ProdColumn reps : repss) (ds : dss) where
  sumParams =
    choose unconsNS inhabited uninhabited
    where
      inhabited =
        sequenceContravariantNP (productParams @(UnconsRep reps)) <> writeNulls2 @repss @dss
      uninhabited =
        writeNulls @reps @ds <> sumParams @repss

sumIndex ::
  Params (NS (NP I) dss)
sumIndex =
  contramap hindex (queryParam @(Prim Auto))

queryParamsNS ::
  ∀ (d :: *) (repss :: [*]) (dss :: [[*]]) .
  ConstructSOP d dss =>
  SumParams repss dss =>
  Params d
queryParamsNS =
  contramap (unSOP . gfrom) (sumIndex <> sumParams @repss)

class ColumnParams (rep :: *) (d :: *) (dss :: [[*]]) where
  columnParams :: Params d

instance (
    ConstructSOP d '[ds],
    ProductParams (UnconsRep reps) ds
  ) => ColumnParams (ProdColumn reps) d '[ds] where
  columnParams =
    queryParamsNP @(UnconsRep reps) @ds

instance (
    ConstructSOP d dss,
    SumParams reps dss
  ) => ColumnParams (SumColumn reps) d dss where
    columnParams =
      queryParamsNS @d @reps @dss

class QueryParams (rep :: *) (d :: *) where
  queryParams :: Params d

instance ColumnParams (ReifyRepTable rep d) d (GCode d) => QueryParams rep d where
  queryParams =
    columnParams @(ReifyRepTable rep d) @d @(GCode d)
