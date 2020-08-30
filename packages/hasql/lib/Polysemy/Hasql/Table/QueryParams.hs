module Polysemy.Hasql.Table.QueryParams where

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
import Polysemy.Hasql.Table.ValueEncoder (ValueEncoder(..))

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

class GenParams reps (ds :: [*]) where
  genParams :: NP Params ds

instance GenParams '[] '[] where
  genParams =
    Nil

instance GenParams Auto '[] where
  genParams =
    Nil

instance (
    QueryParam d,
    GenParams Auto ds
  ) => GenParams Auto (d : ds) where
    genParams =
      queryParam @d :* (genParams @Auto @ds)

instance {-# OVERLAPPABLE #-} (
    QueryParam d,
    GenParams reps ds
  ) => GenParams (r : reps) (d : ds) where
    genParams =
      queryParam @d :* (genParams @reps @ds)

instance (
    QueryParams reps' d,
    GenParams reps ds
  ) => GenParams (Flatten reps' : reps) (d : ds) where
    genParams =
      queryParams @reps' @d :* (genParams @reps @ds)

queryParamsNP ::
  ∀ reps ds d .
  Generic d =>
  Code d ~ '[ds] =>
  GenParams reps ds =>
  Params d
queryParamsNP =
  contramap unpackSOP (mconcat (hcollapse qps))
  where
    unpackSOP =
      unZ . unSOP . from
    qps =
      hzipWith qp (genParams @reps @ds) projections
    qp par (Fn proj) =
      K (contramap (unI . proj . K) par)

class QueryParams rep d where
  queryParams :: Params d

instance
  {-# OVERLAPPABLE #-}
  (
    Generic d,
    Code d ~ '[ds],
    Code rep ~ '[reps],
    GenParams reps ds,
    SameShapeAs reps ds
  ) => QueryParams rep d where
    queryParams =
      queryParamsNP @reps @ds

instance
  (
    Generic d,
    Code d ~ '[ds],
    GenParams Auto ds
  ) => QueryParams Auto d where
    queryParams =
      queryParamsNP @Auto @ds
