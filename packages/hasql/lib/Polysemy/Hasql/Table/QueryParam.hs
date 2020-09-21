module Polysemy.Hasql.Table.QueryParam where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Generics.SOP (All, I, NP, hcpure, unComp, (:.:)(Comp))
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Constraint (ProductCoded)
import Polysemy.Db.SOP.Contravariant (sequenceContravariantNPF)
import Polysemy.Hasql.Table.Representation (ProdColumn)
import Polysemy.Hasql.Table.ValueEncoder (ValueEncoder(valueEncoder))

class QueryParam a where
  queryParam :: Params a

value :: Value a -> Params a
value =
  param . nonNullable

instance ValueEncoder a => QueryParam [a] where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (Vector a) where
  queryParam =
    value $ array (dimension Vector.foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (NonEmpty a) where
  queryParam =
    value $ array (dimension foldl' (element (nonNullable valueEncoder)))
  {-# inline queryParam #-}

instance ValueEncoder a => QueryParam (Maybe a) where
  queryParam =
    param $ nullable valueEncoder
  {-# inline queryParam #-}

instance {-# overlappable #-} ValueEncoder a => QueryParam a where
  queryParam =
    value valueEncoder
  {-# inline queryParam #-}

maybeParam ::
  ∀ a .
  ValueEncoder a =>
  (Params :.: Maybe) a
maybeParam =
  Comp (param (nullable (valueEncoder @a)))

maybeRow ::
  ∀ ds .
  All ValueEncoder ds =>
  Params (NP (I :.: Maybe) ds)
maybeRow =
  sequenceContravariantNPF ps
  where
    ps :: NP (Params :.: Maybe) ds
    ps =
      hcpure (Proxy @ValueEncoder) maybeParam

class NullVariant (rep :: [*]) (ds :: [*]) where
  writeNulls :: Params d

instance NullVariant rep '[] where
  writeNulls =
    mempty

instance {-# overlappable #-} (
    NullVariant reps ds,
    ValueEncoder d
  ) => NullVariant (rep : reps) (d : ds) where
  writeNulls =
    contramap (const Nothing) (unComp (maybeParam @d)) <> writeNulls @reps @ds

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
