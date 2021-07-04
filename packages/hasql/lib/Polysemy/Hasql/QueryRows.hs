module Polysemy.Hasql.QueryRows where

import Generics.SOP (I, NP (Nil, (:*)), NS (S, Z), SListI, SOP (SOP), hsequence)
import Generics.SOP.GGP (gto)
import Hasql.Decoders (Row)
import Polysemy.Db.Data.Rep (Prim)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ProductCoded, ReifySOP)

import Polysemy.Hasql.Table.QueryRow (QueryRow (queryRow))
import Polysemy.Hasql.Table.ReadNull (ReadNullCon (readNullCon), ReadNullCons (readNullCons))

class ProductRows (trees :: [Kind.Tree]) (ds :: [Type]) | trees -> ds where
  productRows :: NP Row ds

instance ProductRows '[] '[] where
  productRows =
    Nil

instance (
    QueryRows tree d,
    ProductRows trees ds
  ) => ProductRows (tree : trees) (d : ds) where
    productRows =
      queryRows @tree :* productRows @trees

class ConRow (tree :: Kind.Con) (ds :: [Type]) | tree -> ds where
  conRow :: NP Row ds

instance (
    ProductRows trees ds
  ) => ConRow ('Kind.Con num n trees) ds where
  conRow =
    productRows @trees

instance (
    QueryRows tree d
  ) => ConRow ('Kind.ConUna num n tree) '[d] where
  conRow =
    queryRows @tree @d :* Nil

class SumRows (trees :: [Kind.Con]) (dss :: [[Type]]) | trees -> dss where
  sumRows :: Int -> Row (NS (NP I) dss)

instance SumRows '[] '[] where
  sumRows index =
    fail [text|invalid index into sum type in database: #{index}|]

instance (
    SListI ds,
    ReadNullCon tree,
    ReadNullCons trees,
    ConRow tree ds,
    SumRows trees dss
  ) => SumRows (tree : trees) (ds : dss) where
  sumRows = \case
    0 ->
      Z <$> hsequence (conRow @tree) <* readNullCons @trees
    index -> do
      readNullCon @tree
      S <$> sumRows @trees @dss (index - 1)

class QueryRows (tree :: Kind.Tree) (d :: Type) | tree -> d where
  queryRows :: Row d

instance (
    ReifySOP d '[ds],
    ProductCoded d ds,
    ProductRows trees ds
  ) => QueryRows ('Kind.Tree n eff ('Kind.Prod d trees)) d where
    queryRows =
      gto . SOP . Z <$> hsequence (productRows @trees @ds)

instance (
    QueryRow eff d
  ) => QueryRows ('Kind.Tree n eff ('Kind.Prim d)) d where
  queryRows =
    queryRow @eff @d

instance (
    ReifySOP d dss,
    SumRows trees dss
  ) => QueryRows ('Kind.Tree n eff ('Kind.SumProd d trees)) d where
  queryRows =
    gto . SOP <$> (sumRows @trees @dss =<< queryRow @'[Prim])
