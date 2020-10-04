module Polysemy.Hasql.Table.QueryRows where

import Generics.SOP (
  All,
  I,
  NP((:*), Nil),
  NS(Z, S),
  SOP(SOP),
  Top,
  hsequence,
  )
import Generics.SOP.GGP (GCode, gto)
import Hasql.Decoders (Row)
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.SOP.Constraint (ReifySOP)
import Polysemy.Hasql.Table.ColumnType (Done, Multi, Single, UnconsRep)
import Polysemy.Hasql.Table.QueryRow (NullVariant, NullVariants, QueryRow(queryRow), readNulls, readNulls2)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)

class ProductRows (rep :: *) (ds :: [*]) where
  genRows :: NP Row ds

instance ProductRows Done '[] where
  genRows =
    Nil

instance (
    QueryRow rep d,
    ProductRows (UnconsRep reps) ds
  ) => ProductRows (Single rep reps) (d : ds) where
    genRows =
      queryRow @rep @d :* genRows @(UnconsRep reps) @ds

instance (
    ColumnRows head d (GCode d),
    ProductRows (UnconsRep tail) ds
  ) => ProductRows (Multi head tail) (d : ds) where
    genRows =
      genQueryRows @head @d @(GCode d) :* genRows @(UnconsRep tail) @ds

class SumRows (repss :: [*]) (dss :: [[*]]) where
  sumRows :: Int -> Row (NS (NP I) dss)

instance SumRows '[] '[] where
  sumRows _ =
    fail "invalid index into sum type in database"

instance (
    All Top ds,
    NullVariant (ProdColumn reps) ds,
    NullVariants repss dss,
    ProductRows (UnconsRep reps) ds,
    SumRows repss dss
  ) => SumRows (ProdColumn reps : repss) (ds : dss) where
  sumRows = \case
    0 ->
      Z <$> hsequence (genRows @(UnconsRep reps) @ds) <* readNulls2 @repss @dss
    index -> do
      readNulls @(ProdColumn reps) @ds
      S <$> sumRows @repss @dss (index - 1)

class ColumnRows (repss :: *) (d :: *) (dss :: [[*]]) where
  genQueryRows :: Row d

instance (
    ReifySOP d '[ds],
    ProductRows (UnconsRep reps) ds
  ) => ColumnRows (ProdColumn reps) d '[ds] where
    genQueryRows =
      gto . SOP . Z <$> hsequence (genRows @(UnconsRep reps) @ds)

instance (
    ReifySOP d dss,
    SumRows repss dss
  ) => ColumnRows (SumColumn repss) d dss where
    genQueryRows =
      gto . SOP <$> (sumRows @repss =<< queryRow @(Prim Auto))

class QueryRows rep d where
  queryRows :: Row d

instance ColumnRows (ReifyRepTable rep d) d (GCode d) => QueryRows rep d where
    queryRows =
      genQueryRows @(ReifyRepTable rep d) @d @(GCode d)
