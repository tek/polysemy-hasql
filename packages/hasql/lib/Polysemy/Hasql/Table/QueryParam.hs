module Polysemy.Hasql.Table.QueryParam where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Constraint (ProductCoded)
import Polysemy.Hasql.Table.Representation (ProdColumn)
import Polysemy.Hasql.Table.ValueEncoder (RepEncoder, repEncoder)

class QueryParam (rep :: *) (a :: *) where
  queryParam :: Params a

value :: Value a -> Params a
value =
  param . nonNullable

instance RepEncoder r a => QueryParam r [a] where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable (repEncoder @r))))
  {-# inline queryParam #-}

instance RepEncoder r a => QueryParam r (Vector a) where
  queryParam =
    value $ array (dimension Vector.foldl' (element (nonNullable (repEncoder @r))))
  {-# inline queryParam #-}

instance RepEncoder r a => QueryParam r (NonEmpty a) where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable (repEncoder @r))))
  {-# inline queryParam #-}

instance RepEncoder r a => QueryParam r (Maybe a) where
  queryParam =
    param $ nullable (repEncoder @r)
  {-# inline queryParam #-}

instance {-# overlappable #-} RepEncoder r a => QueryParam r a where
  queryParam =
    value (repEncoder @r)
  {-# inline queryParam #-}

type family NullParam a :: * where
  NullParam (Maybe a) = Maybe a
  NullParam a = Maybe a

maybeParam ::
  ∀ rep a .
  NullParam a ~ Maybe a =>
  QueryParam rep (NullParam a) =>
  Params (Maybe a)
maybeParam =
  queryParam @rep @(NullParam a)

class NullVariant (rep :: [*]) (ds :: [*]) where
  writeNulls :: Params d

instance NullVariant rep '[] where
  writeNulls =
    mempty

instance {-# overlappable #-} (
    NullVariant reps ds,
    NullParam d ~ Maybe a,
    NullParam a ~ Maybe a,
    QueryParam rep (NullParam d)
  ) => NullVariant (rep : reps) (d : ds) where
  writeNulls =
    contramap (const Nothing) (maybeParam @rep @a) <> writeNulls @reps @ds

instance (
    ProductCoded d dSub,
    NullVariant rSub dSub,
    NullVariant reps ds
  ) => NullVariant (Flatten (ProdColumn rSub) : reps) (d : ds) where
  writeNulls =
    writeNulls @rSub @dSub <> writeNulls @reps @ds

class NullVariants (reps :: [*]) (dss :: [[*]]) where
  writeNulls2 :: Params a

instance NullVariants reps '[] where
  writeNulls2 =
    mempty

instance (
    NullVariant rep ds,
    NullVariants reps dss
  ) => NullVariants (ProdColumn rep : reps) (ds : dss) where
  writeNulls2 =
    writeNulls @rep @ds <> writeNulls2 @reps @dss
