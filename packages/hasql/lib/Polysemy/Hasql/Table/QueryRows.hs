module Polysemy.Hasql.Table.QueryRows where

import Generics.SOP (All,
  Code,
  Generic,
  I,
  NP((:*), Nil),
  NS(Z, S),
  SOP(SOP),
  Top,
  hsequence,
  to,
  )
import Hasql.Decoders (Row)
import Prelude hiding (All, Generic)

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, Sum)
import Polysemy.Hasql.Table.QueryRow (QueryRow(queryRow))

class GenRows rep (ds :: [*]) where
  genRows :: NP Row ds

instance GenRows '[] '[] where
  genRows =
    Nil

instance GenRows Auto '[] where
  genRows =
    Nil

instance (
    QueryRow d,
    GenRows Auto ds
  ) => GenRows Auto (d : ds) where
    genRows =
      queryRow @d :* (genRows @Auto @ds)

instance (
    QueryRow d,
    GenRows reps ds
  ) => GenRows (Auto : reps) (d : ds) where
    genRows =
      queryRow @d :* (genRows @reps @ds)

instance (
    QueryRow d,
    GenRows reps ds
  ) => GenRows (Prim flags : reps) (d : ds) where
    genRows =
      queryRow @d :* (genRows @reps @ds)

instance (
    QueryRows rep d,
    GenRows reps ds
  ) => GenRows (Sum rep : reps) (d : ds) where
    genRows =
      queryRows @rep @d :* (genRows @reps @ds)

instance (
    QueryRows reps' d,
    GenRows reps ds
  ) => GenRows (Flatten reps' : reps) (d : ds) where
    genRows =
      queryRows @reps' @d :* (genRows @reps @ds)

class Nulls2 (ds :: [*]) where
  nulls2 :: Row ()

instance Nulls2 '[] where
  nulls2 =
    unit

-- doing this with 'hcpure' seems to send ghc spinning because of the necessity of the constraint being @QueryRow d@
-- instead of @QueryRow (Maybe d)@
instance (
    Nulls2 ds,
    QueryRow (Maybe d)
  ) => Nulls2 (d : ds) where
  nulls2 =
    void (queryRow @(Maybe d)) *> nulls2 @ds

class SumRows (repss :: [[*]]) (dss :: [[*]]) where
  sumRows :: Int -> Row (NS (NP I) dss)

instance SumRows '[] '[] where
  sumRows _ =
    fail "invalid index into sum type in database"

instance (
    All Top ds,
    GenRows reps ds,
    Nulls2 ds,
    SumRows repss dss
  ) => SumRows (reps : repss) (ds : dss) where
  sumRows = \case
    0 ->
      Z <$> hsequence (genRows @reps @ds)
    index -> do
      nulls2 @ds
      S <$> sumRows @repss @dss (index - 1)

genRowNP ::
  ∀ reps ds d .
  Generic d =>
  Code d ~ '[ds] =>
  GenRows reps ds =>
  Row d
genRowNP =
  to . SOP . Z <$> hsequence (genRows @reps @ds)

genRowNS ::
  ∀ (a :: *) repss (dss :: [[*]]) .
  Generic a =>
  Code a ~ dss =>
  SumRows repss dss =>
  Row a
genRowNS =
  to . SOP <$> (sumRows @repss =<< queryRow)

class GenQueryRows repss (d :: *) (dss :: [[*]]) where
  genQueryRows :: Row d

instance (
    Generic d,
    Code d ~ '[ds],
    GenRows reps ds
  ) => GenQueryRows '[reps] d '[ds] where
    genQueryRows =
      genRowNP @reps @ds

instance (
    Generic d,
    Code d ~ (d1 : d2 : dss),
    SumRows (r1 : r2 : repss) (d1 : d2 : dss)
  ) => GenQueryRows (r1 : r2 : repss) d (d1 : d2 : dss) where
    genQueryRows =
      genRowNS @d @(r1 : r2 : repss) @(d1 : d2 : dss)

class QueryRows rep d where
  queryRows :: Row d

instance {-# overlappable #-} (
    Generic d,
    GenQueryRows (Code rep) d (Code d)
  ) => QueryRows rep d where
    queryRows =
      genQueryRows @(Code rep) @d @(Code d)

instance (
    Generic d,
    Code d ~ '[ds],
    GenRows Auto ds
  ) => QueryRows Auto d where
    queryRows =
      genRowNP @Auto @ds
