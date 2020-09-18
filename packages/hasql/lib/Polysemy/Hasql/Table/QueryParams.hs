module Polysemy.Hasql.Table.QueryParams where

import Data.Functor.Contravariant.Divisible (choose)
import Generics.SOP (All,
  Code,
  Generic,
  I(I),
  K(K),
  NP(Nil, (:*)),
  NS,
  NS(Z, S),
  Projection,
  SameShapeAs,
  Top,
  from,
  hcollapse,
  hindex,
  hmap,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unComp,
  unI,
  unSOP,
  unZ,
  (:.:),
  (:.:)(Comp),
  )
import Hasql.Encoders (Params)
import Prelude hiding (All, Generic)

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, Sum)
import Polysemy.Hasql.Table.QueryParam (Null2, QueryParam(queryParam), allNulls, nullsSum)
import Polysemy.Hasql.Table.ValueEncoder (ValueEncoder)

class GenParams reps (ds :: [*]) where
  genParams :: NP Params ds

instance GenParams '[] '[] where
  genParams =
    Nil

instance GenParams Auto '[] where
  genParams =
    Nil

instance (
    QueryParam d,
    GenParams Auto ds
  ) => GenParams Auto (d : ds) where
    genParams =
      queryParam @d :* genParams @Auto @ds

instance {-# overlappable #-} (
    QueryParam d,
    GenParams reps ds
  ) => GenParams (r : reps) (d : ds) where
    genParams =
      queryParam @d :* genParams @reps @ds

instance (
    QueryParam d,
    GenParams reps ds
  ) => GenParams (Prim r : reps) (d : ds) where
    genParams =
      queryParam @d :* genParams @reps @ds

instance (
    QueryParams r d,
    GenParams reps ds
  ) => GenParams (Sum r : reps) (d : ds) where
    genParams =
      queryParams @r @d :* genParams @reps @ds

instance (
    QueryParams reps' d,
    GenParams reps ds
  ) => GenParams (Flatten reps' : reps) (d : ds) where
    genParams =
      queryParams @reps' @d :* genParams @reps @ds

queryParamsNP ::
  ∀ reps ds d .
  Generic d =>
  Code d ~ '[ds] =>
  GenParams reps ds =>
  Params d
queryParamsNP =
  contramap unpackSOP res
  where
    res =
      mconcat clps
    clps =
      hcollapse qps
    unpackSOP =
      unZ . unSOP . from
    qps :: NP (K (Params (NP I ds))) ds
    qps =
      hzipWith qp (genParams @reps @ds :: NP Params ds) (projections :: NP (Projection I ds) ds)
    qp :: ∀ a . Params a -> Projection I ds a -> K (Params (NP I ds)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

sequenceContravariantNPF ::
  ∀ (ds :: [*]) (cv :: * -> *) (f :: * -> *) .
  All Top ds =>
  Contravariant cv =>
  (∀ a . Monoid (cv a)) =>
  NP (cv :.: f) ds ->
  cv (NP (I :.: f) ds)
sequenceContravariantNPF contrs =
  mconcat (hcollapse rows)
  where
    rows =
      hzipWith row contrs projections
    row :: ∀ a . (cv :.: f) a -> Projection f ds a -> K (cv (NP (I :.: f) ds)) a
    row (Comp par) (Fn proj) =
      K (contramap (proj . K . hmap (unI . unComp)) par)

sequenceContravariantNP ::
  ∀ (ds :: [*]) (cv :: * -> *) .
  All Top ds =>
  Contravariant cv =>
  (∀ a . Monoid (cv a)) =>
  NP cv ds ->
  cv (NP I ds)
sequenceContravariantNP =
  contramap (hmap (Comp .  I)) .
  sequenceContravariantNPF .
  hmap (Comp . contramap unI)

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

class SumParams (repss :: [[*]]) (dss :: [[*]]) where
  sumParams :: Params (NS (NP I) dss)

instance SumParams '[] '[] where
  sumParams =
    mempty

instance (
  All Top ds,
  All ValueEncoder ds,
  Null2 (NP I ds) dss,
  SumParams repss dss,
  GenParams reps ds
  ) => SumParams (reps : repss) (ds : dss) where
  sumParams =
    choose unconsNS (sequenceContravariantNP (genParams @reps) <> allNulls @ds @dss) (nullsSum @ds <> sumParams @repss)

sumIndex ::
  Params (NS (NP I) dss)
sumIndex =
  contramap hindex queryParam

queryParamsNS ::
  ∀ (a :: *) (repss :: [[*]]) (dss :: [[*]]) .
  Generic a =>
  Code a ~ dss =>
  SumParams repss dss =>
  Params a
queryParamsNS =
  contramap (unSOP . from) (sumIndex <> sumParams @repss)

class GenQueryParams repss d dss where
  genQueryParams :: Params d

instance (
    Generic d,
    Code d ~ '[ds],
    GenParams reps ds,
    SameShapeAs reps ds
  ) => GenQueryParams '[reps] d '[ds] where
  genQueryParams =
    queryParamsNP @reps @ds

instance (
    Generic d,
    Code d ~ (d1 : d2 : dss),
    SumParams (r1 : r2 : repss) (d1 : d2 : dss)
  ) => GenQueryParams (r1 : r2 : repss) d (d1 : d2 : dss) where
    genQueryParams =
      queryParamsNS @d @(r1 : r2 : repss) @(d1 : d2 : dss)

class QueryParams (rep :: *) (d :: *) where
  queryParams :: Params d

instance {-# overlappable #-} (
    Generic d,
    GenQueryParams (Code rep) d (Code d)
  ) => QueryParams rep d where
    queryParams =
      genQueryParams @(Code rep) @d @(Code d)

instance (
    Generic d,
    Code d ~ '[ds],
    GenParams Auto ds
  ) => QueryParams Auto d where
    queryParams =
      queryParamsNP @Auto @ds
