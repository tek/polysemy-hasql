module Polysemy.Hasql.QueryParams where

import Data.Functor.Contravariant.Divisible (choose)
import Generics.SOP (
  All,
  I,
  K(..),
  NP(..),
  NS(..),
  Projection,
  SListI,
  Top,
  hcollapse,
  hd,
  hindex,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unI,
  unSOP,
  unZ,
  )
import Generics.SOP.GGP (gfrom)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Prim)
import Polysemy.Db.Data.PartialFields (FieldTypes, PartialFields)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, ProductCoded)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNP)

import Polysemy.Hasql.Table.QueryParam (QueryParam(queryParam))
import Polysemy.Hasql.Table.WriteNull (WriteNullCon(writeNullCon), WriteNullCons(writeNullCons))

class ProductParams (trees :: [Kind.Tree]) (ds :: [*]) | trees -> ds where
  productParams :: NP Params ds

instance ProductParams '[] '[] where
  productParams =
    Nil

instance (
    QueryParams tree d,
    ProductParams trees ds
  ) => ProductParams (tree : trees) (d : ds) where
  productParams =
    queryParams @tree :* productParams @trees

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

class ConParams (tree :: Kind.Con) (ds :: [*]) | tree -> ds where
  conParams :: Params (NP I ds)

instance (
    SListI ds,
    ProductParams tree ds
  ) => ConParams ('Kind.Con n tree) ds where
  conParams =
    sequenceContravariantNP (productParams @tree)

instance (
    QueryParam eff d
  ) => ConParams ('Kind.ConUna n ('Kind.Tree _n eff ('Kind.Prim d))) '[d] where
  conParams =
    unI . hd >$< queryParam @eff

class SumParams (trees :: [Kind.Con]) (dss :: [[*]]) | trees -> dss where
  sumParams :: Params (NS (NP I) dss)

instance SumParams '[] '[] where
  sumParams =
    mempty

instance (
    SListI ds,
    WriteNullCon tree,
    WriteNullCons trees,
    ConParams tree ds,
    SumParams trees dss
  ) => SumParams (tree : trees) (ds : dss) where
  sumParams =
    choose unconsNS inhabited uninhabited
    where
      inhabited =
        conParams @tree <> writeNullCons @trees
      uninhabited =
        writeNullCon @tree <> sumParams @trees

paramsNP ::
  ∀ ds trees .
  All Top ds =>
  ProductParams trees ds =>
  Params (NP I ds)
paramsNP =
  mconcat (hcollapse qps)
  where
    qps :: NP (K (Params (NP I ds))) ds
    qps =
      hzipWith qp (productParams @trees :: NP Params ds) (projections :: NP (Projection I ds) ds)
    qp :: ∀ a . Params a -> Projection I ds a -> K (Params (NP I ds)) a
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

sumIndex ::
  Params (NS (NP I) dss)
sumIndex =
  contramap hindex (queryParam @'[Prim])

class QueryParams (tree :: Kind.Tree) (d :: *) | tree -> d where
  queryParams :: Params d

instance (
    QueryParam eff d
  ) => QueryParams ('Kind.Tree n eff ('Kind.Prim d)) d where
  queryParams =
    queryParam @eff @d

instance (
    ProductCoded d ds,
    ConstructSOP d '[ds],
    ProductParams trees ds
  ) => QueryParams ('Kind.Tree n eff ('Kind.Prod d trees)) d where
  queryParams =
    contramap (unZ . unSOP . gfrom) (paramsNP @ds @trees)

instance (
    ConstructSOP d dss,
    SumParams trees dss
  ) => QueryParams ('Kind.Tree n eff ('Kind.SumProd d trees)) d where
    queryParams =
      unSOP . gfrom >$< (sumIndex <> sumParams @trees)

class PartialQueryParams (tree :: Kind.Tree) (d :: *) where
  partialQueryParams :: Params (PartialFields d)

instance (
    ds ~ FieldTypes d,
    All Top ds,
    ProductParams trees ds
  ) => PartialQueryParams ('Kind.Tree n eff ('Kind.Prod d trees)) d where
  partialQueryParams =
    undefined >$< paramsNP @ds @trees

instance PartialQueryParams ('Kind.Tree n eff ('Kind.Sum d trees)) d where
  partialQueryParams =
    mempty
