module Polysemy.Hasql.QueryParams where

import Data.Functor.Contravariant.Divisible (choose)
import Generics.SOP (
  HCollapse(hcollapse),
  HIndex(hindex),
  I,
  K(..),
  NP(..),
  NS(..),
  Projection,
  SListI,
  hd,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unI,
  unSOP,
  unZ,
  )
import Generics.SOP.GGP (gfrom)
import Hasql.Encoders (Params)
import Polysemy.Db.SOP.Constraint (ProductCoded)

import Polysemy.Db.Data.Column (Prim)
import Polysemy.Db.SOP.Constraint (ConstructSOP)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNP)
import Polysemy.Hasql.Column.Class (SumIndexColumn)
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind
import Polysemy.Hasql.Table.QueryParam (QueryParam(queryParam))
import Polysemy.Hasql.Table.WriteNull (WriteNullCon(writeNullCon), WriteNullCons(writeNullCons))

class ProductParams (ds :: [*]) (cs :: [Kind.Column]) where
  productParams :: NP Params ds

instance ProductParams '[] '[] where
  productParams =
    Nil

instance (
    QueryParam eff d,
    ProductParams ds cs
  ) => ProductParams (d : ds) ('Kind.Column n eff ('Kind.Prim d) : cs) where
  productParams =
    queryParam @eff @d :* productParams @ds @cs

instance {-# overlappable #-} (
    QueryParams c d,
    ProductParams ds cs
  ) => ProductParams (d : ds) (c : cs) where
  productParams =
    queryParams @c @d :* productParams @ds @cs

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

class ConParams (ds :: [*]) (c :: Kind.Column) where
  conParams :: Params (NP I ds)

instance (
    SListI ds,
    ProductParams ds c
  ) => ConParams ds ('Kind.Column n eff ('Kind.Prod d c)) where
  conParams =
    sequenceContravariantNP (productParams @ds @c)

instance (
    QueryParam eff d
  ) => ConParams '[d] ('Kind.Column n eff ('Kind.Prim d)) where
  conParams =
    unI . hd >$< queryParam @eff @d

class SumParams (dss :: [[*]]) (cs :: [Kind.Column]) where
  sumParams :: Params (NS (NP I) dss)

instance SumParams '[] '[] where
  sumParams =
    mempty

instance (
    SListI ds,
    WriteNullCon ds c,
    WriteNullCons dss cs,
    ConParams ds c,
    SumParams dss cs
  ) => SumParams (ds : dss) (c : cs) where
  sumParams =
    choose unconsNS inhabited uninhabited
    where
      inhabited =
        conParams @ds @c <> writeNullCons @dss @cs
      uninhabited =
        writeNullCon @ds @c <> sumParams @dss @cs

queryParamsNP ::
  ∀ ds cs d .
  ProductParams ds cs =>
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
      hzipWith qp (productParams @ds @cs :: NP Params ds) (projections :: NP (Projection I ds) ds)
    qp :: ∀ a . Params a -> Projection I ds a -> K (Params (NP I ds)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

sumIndex ::
  Params (NS (NP I) dss)
sumIndex =
  contramap hindex (queryParam @'[Prim])

class QueryParams (rep :: Kind.Column) (d :: *) where
  queryParams :: Params d

instance (
    QueryParam eff d
  ) => QueryParams ('Kind.Column n eff ('Kind.Prim d)) d where
  queryParams =
    queryParam @eff @d

instance (
    ProductCoded d ds,
    ConstructSOP d '[ds],
    ProductParams ds cs
  ) => QueryParams ('Kind.Column n eff ('Kind.Prod d cs)) d where
  queryParams =
    queryParamsNP @ds @cs @d

instance (
    ConstructSOP d dss,
    SumParams dss cs
  ) => QueryParams ('Kind.Column n eff ('Kind.Sum d (SumIndexColumn : cs))) d where
    queryParams =
      unSOP . gfrom >$< (sumIndex <> sumParams @dss @cs)