module Polysemy.Hasql.Table.Row where

import Generics.SOP hiding (FieldInfo)
import Hasql.Decoders (Row)
import Prelude hiding (Generic)

import Polysemy.Db.Data.Column (Auto, Flatten, Prim)
import Polysemy.Hasql.Table.QueryRow (QueryRow(queryRow))

class GenRow' rep (ds :: [*]) where
  genRow' :: NP Row ds

instance GenRow' '[] '[] where
  genRow' =
    Nil

instance GenRow' Auto '[] where
  genRow' =
    Nil

instance (
    QueryRow d,
    GenRow' Auto ds
  ) => GenRow' Auto (d : ds) where
    genRow' =
      queryRow @d :* (genRow' @Auto @ds)

instance (
    QueryRow d,
    GenRow' reps ds
  ) => GenRow' (Auto : reps) (d : ds) where
    genRow' =
      queryRow @d :* (genRow' @reps @ds)

instance (
    QueryRow d,
    GenRow' reps ds
  ) => GenRow' (Prim flags : reps) (d : ds) where
    genRow' =
      queryRow @d :* (genRow' @reps @ds)

instance (
    GenRow reps' d,
    GenRow' reps ds
  ) => GenRow' (Flatten reps' : reps) (d : ds) where
    genRow' =
      genRow @reps' @d :* (genRow' @reps @ds)

genRowNP ::
  ∀ reps ds d .
  Generic d =>
  Code d ~ '[ds] =>
  GenRow' reps ds =>
  Row d
genRowNP =
  to . SOP . Z <$> hsequence (genRow' @reps @ds)

class GenRow rep d where
  genRow :: Row d

instance {-# OVERLAPPABLE #-} (
    Generic d,
    Code d ~ '[ds],
    Code rep ~ '[reps],
    GenRow' reps ds
  ) => GenRow rep d where
    genRow =
      genRowNP @reps @ds

instance (
    Generic d,
    Code d ~ '[ds],
    GenRow' Auto ds
  ) => GenRow Auto d where
    genRow =
      genRowNP @Auto @ds
