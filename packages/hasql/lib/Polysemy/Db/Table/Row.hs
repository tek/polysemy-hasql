module Polysemy.Db.Table.Row where

import Generics.SOP hiding (FieldInfo)
import Hasql.Decoders (Row)
import Prelude hiding (Generic)

import Polysemy.Db.Data.Column (Auto, Flatten, Prim)
import Polysemy.Db.Table.QueryRow (QueryRow(queryRow))

class GenRow' (ds :: [*]) rep where
  genRow' :: NP Row ds

instance GenRow' '[] '[] where
  genRow' =
    Nil

instance GenRow' '[] Auto where
  genRow' =
    Nil

instance (
    QueryRow d,
    GenRow' ds Auto
  ) => GenRow' (d : ds) Auto where
    genRow' =
      queryRow @d :* (genRow' @ds @Auto)

instance (
    QueryRow d,
    GenRow' ds reps
  ) => GenRow' (d : ds) (Auto : reps) where
    genRow' =
      queryRow @d :* (genRow' @ds @reps)

instance (
    QueryRow d,
    GenRow' ds reps
  ) => GenRow' (d : ds) (Prim flags : reps) where
    genRow' =
      queryRow @d :* (genRow' @ds @reps)

instance (
    GenRow d reps',
    GenRow' ds reps
  ) => GenRow' (d : ds) (Flatten reps' : reps) where
    genRow' =
      genRow @d @reps' :* (genRow' @ds @reps)

genRowNP ::
  ∀ ds reps d .
  Generic d =>
  Code d ~ '[ds] =>
  GenRow' ds reps =>
  Row d
genRowNP =
  to . SOP . Z <$> hsequence (genRow' @ds @reps)

class GenRow d rep where
  genRow :: Row d

instance {-# OVERLAPPABLE #-} (
    Generic d,
    Code d ~ '[ds],
    Code rep ~ '[reps],
    GenRow' ds reps
  ) => GenRow d rep where
    genRow =
      genRowNP @ds @reps

instance (
    Generic d,
    Code d ~ '[ds],
    GenRow' ds Auto
  ) => GenRow d Auto where
    genRow =
      genRowNP @ds @Auto
