module Polysemy.Hasql.QueryRows where

import Generics.SOP (I, NP((:*), Nil), NS(Z, S), SListI, SOP(SOP), hsequence)
import Generics.SOP.GGP (gto)
import Hasql.Decoders (Row)
import Polysemy.Db.Data.Column (Prim)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ProductCoded, ReifySOP)

import Polysemy.Hasql.Column.Class (SumIndexColumn)
import Polysemy.Hasql.Table.QueryRow (QueryRow(queryRow))
import Polysemy.Hasql.Table.ReadNull (ReadNullCon(readNullCon), ReadNullCons(readNullCons))

class ProductRows (ds :: [*]) (cs :: [Kind.Tree]) where
  productRows :: NP Row ds

instance ProductRows '[] '[] where
  productRows =
    Nil

instance (
    QueryRow eff d,
    ProductRows ds cs
  ) => ProductRows (d : ds) ('Kind.Tree name eff ('Kind.Prim d) : cs) where
  productRows =
    queryRow @eff @d :* productRows @ds @cs

instance {-# overlappable #-} (
    QueryRows c d,
    ProductRows ds cs
  ) => ProductRows (d : ds) (c : cs) where
    productRows =
      queryRows @c @d :* productRows @ds @cs

class ConRow (ds :: [*]) (c :: Kind.Tree) where
  conRow :: NP Row ds

instance (
    ProductRows ds c
  ) => ConRow ds ('Kind.Tree n eff ('Kind.Prod d c)) where
  conRow =
    productRows @ds @c

instance (
    QueryRow eff d
  ) => ConRow '[d] ('Kind.Tree n eff ('Kind.Prim d)) where
  conRow =
    queryRow @eff @d :* Nil

class SumRows (dss :: [[*]]) (cs :: [Kind.Tree]) where
  sumRows :: Int -> Row (NS (NP I) dss)

instance SumRows '[] '[] where
  sumRows index =
    fail [qt|invalid index into sum type in database: #{index}|]

instance (
    SListI ds,
    ReadNullCon c,
    ReadNullCons cs,
    ConRow ds c,
    SumRows dss cs
  ) => SumRows (ds : dss) (c : cs) where
  sumRows = \case
    0 ->
      Z <$> hsequence (conRow @ds @c) <* readNullCons @cs
    index -> do
      readNullCon @c
      S <$> sumRows @dss @cs (index - 1)

class QueryRows (rep :: Kind.Tree) (d :: *) where
  queryRows :: Row d

instance (
    ReifySOP d '[ds],
    ProductCoded d ds,
    ProductRows ds cs
  ) => QueryRows ('Kind.Tree n eff ('Kind.Prod d cs)) d where
    queryRows =
      gto . SOP . Z <$> hsequence (productRows @ds @cs)

instance (
    ReifySOP d dss,
    SumRows dss cs
  ) => QueryRows ('Kind.Tree n eff ('Kind.Sum d (SumIndexColumn : cs))) d where
  queryRows =
    gto . SOP <$> (sumRows @dss @cs =<< queryRow @'[Prim])
