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

import Polysemy.Db.Data.Column (Flatten, Prim, Sum)
import Polysemy.Db.SOP.Constraint (ProductCoded, ReifySOP)
import Polysemy.Hasql.Table.QueryRow (QueryRow(queryRow))
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, ReifySumType, SumColumn)

class GenRows (rep :: [*]) (ds :: [*]) where
  genRows :: NP Row ds

instance GenRows '[] '[] where
  genRows =
    Nil

instance (
    QueryRow d,
    GenRows reps ds
  ) => GenRows (Prim flags : reps) (d : ds) where
    genRows =
      queryRow @d :* genRows @reps @ds

instance (
    GenQueryRows (ReifySumType rep d) d (GCode d),
    GenRows reps ds
  ) => GenRows (Sum rep : reps) (d : ds) where
    genRows =
      genQueryRows @(ReifySumType rep d) @d @(GCode d) :* genRows @reps @ds

instance (
    GenQueryRows rep d (GCode d),
    GenRows reps ds
  ) => GenRows (Flatten rep : reps) (d : ds) where
    genRows =
      genQueryRows @rep @d @(GCode d) :* genRows @reps @ds

class NullVariants reps (ds :: [*]) where
  readNulls2 :: Row ()

instance NullVariants rep '[] where
  readNulls2 =
    unit

-- doing this with 'hcpure' seems to send ghc spinning because of the necessity of the constraint being @QueryRow d@
-- instead of @QueryRow (Maybe d)@

-- instance {-# overlappable #-} (
--     NullVariants reps ds,
--     QueryRow (Maybe d)
--   ) => NullVariants (rep : reps) (d : ds) where
--   readNulls2 =
--     void (queryRow @(Maybe d)) *> readNulls2 @reps @ds

instance (
    NullVariants (ProdColumn reps) ds,
    QueryRow (Maybe d)
  ) => NullVariants (ProdColumn (rep : reps)) (Maybe d : ds) where
  readNulls2 =
    void (queryRow @(Maybe d)) *> readNulls2 @(ProdColumn reps) @ds

instance {-# overlappable #-} (
    NullVariants (ProdColumn reps) ds,
    QueryRow (Maybe d)
  ) => NullVariants (ProdColumn (rep : reps)) (d : ds) where
  readNulls2 =
    void (queryRow @(Maybe d)) *> readNulls2 @(ProdColumn reps) @ds

instance (
    ProductCoded d dSub,
    NullVariants rSub dSub,
    NullVariants reps ds
  ) => NullVariants (ProdColumn (Flatten rSub : reps)) (d : ds) where
  readNulls2 =
    readNulls2 @rSub @dSub *> readNulls2 @reps @ds

class SumRows (repss :: [*]) (dss :: [[*]]) where
  sumRows :: Int -> Row (NS (NP I) dss)

instance SumRows '[] '[] where
  sumRows _ =
    fail "invalid index into sum type in database"

instance (
    All Top ds,
    NullVariants (ProdColumn reps) ds,
    GenRows reps ds,
    SumRows repss dss
  ) => SumRows (ProdColumn reps : repss) (ds : dss) where
  sumRows = \case
    0 ->
      Z <$> hsequence (genRows @reps @ds)
    index -> do
      readNulls2 @(ProdColumn reps) @ds
      S <$> sumRows @repss @dss (index - 1)

class GenQueryRows (repss :: *) (d :: *) (dss :: [[*]]) where
  genQueryRows :: Row d

instance (
    ReifySOP d '[ds],
    GenRows reps ds
  ) => GenQueryRows (ProdColumn reps) d '[ds] where
    genQueryRows =
      gto . SOP . Z <$> hsequence (genRows @reps @ds)

instance (
    ReifySOP d dss,
    SumRows repss dss
  ) => GenQueryRows (SumColumn repss) d dss where
    genQueryRows =
      gto . SOP <$> (sumRows @repss =<< queryRow)

class QueryRows rep d where
  queryRows :: Row d

instance GenQueryRows (ReifyRepTable rep d) d (GCode d) => QueryRows rep d where
    queryRows =
      genQueryRows @(ReifyRepTable rep d) @d @(GCode d)
