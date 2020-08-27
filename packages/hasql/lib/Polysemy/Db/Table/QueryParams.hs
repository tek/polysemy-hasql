module Polysemy.Db.Table.QueryParams where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Generics.SOP (
  Code,
  Generic,
  K(K),
  NP(Nil, (:*)),
  SameShapeAs,
  from,
  hcollapse,
  hzipWith,
  projections,
  type (-.->)(Fn),
  unI,
  unSOP,
  unZ,
  )
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)
import Prelude hiding (All, Generic)

import Polysemy.Db.Data.Column (Auto, Flatten)
import Polysemy.Db.Table.ValueEncoder (ValueEncoder(..))

class QueryParam a where
  queryParam :: Params a

value :: Value a -> Params a
value =
  param . nonNullable

instance ValueEncoder a => QueryParam [a] where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# INLINE queryParam #-}

instance ValueEncoder a => QueryParam (Vector a) where
  queryParam =
    value $ array (dimension Vector.foldl' (element (nonNullable valueEncoder)))
  {-# INLINE queryParam #-}

instance ValueEncoder a => QueryParam (NonEmpty a) where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# INLINE queryParam #-}

instance ValueEncoder a => QueryParam (Maybe a) where
  queryParam =
    param $ nullable valueEncoder
  {-# INLINE queryParam #-}

instance {-# OVERLAPPABLE #-} ValueEncoder a => QueryParam a where
  queryParam =
    value valueEncoder
  {-# INLINE queryParam #-}

class GenParams (ds :: [*]) reps where
  genParams :: NP Params ds

instance GenParams '[] '[] where
  genParams =
    Nil

instance GenParams '[] Auto where
  genParams =
    Nil

instance (
    QueryParam d,
    GenParams ds Auto
  ) => GenParams (d : ds) Auto where
    genParams =
      queryParam @d :* (genParams @ds @Auto)

instance {-# OVERLAPPABLE #-} (
    QueryParam d,
    GenParams ds reps
  ) => GenParams (d : ds) (r : reps) where
    genParams =
      queryParam @d :* (genParams @ds @reps)

instance (
    QueryParams d reps',
    GenParams ds reps
  ) => GenParams (d : ds) (Flatten reps' : reps) where
    genParams =
      queryParams @d @reps' :* (genParams @ds @reps)

queryParamsNP ::
  ∀ ds reps d .
  Generic d =>
  Code d ~ '[ds] =>
  GenParams ds reps =>
  Params d
queryParamsNP =
  contramap unpackSOP (mconcat (hcollapse qps))
  where
    unpackSOP =
      unZ . unSOP . from
    qps =
      hzipWith qp (genParams @ds @reps) projections
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

class QueryParams d rep where
  queryParams :: Params d

instance
  {-# OVERLAPPABLE #-}
  (
    Generic d,
    Code d ~ '[ds],
    Code rep ~ '[reps],
    GenParams ds reps,
    SameShapeAs ds reps
  ) => QueryParams d rep where
    queryParams =
      queryParamsNP @ds @reps

instance
  (
    Generic d,
    Code d ~ '[ds],
    GenParams ds Auto
  ) => QueryParams d Auto where
    queryParams =
      queryParamsNP @ds @Auto
