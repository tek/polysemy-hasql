module Polysemy.Hasql.Table.QueryParam where

import Data.Vector (Vector)
import Hasql.Encoders (Params, Value, array, dimension, element, nonNullable, nullable, param)

import Polysemy.Hasql.Table.EncoderValue (EncoderValue, encoderValue)
import Polysemy.Db.Tree.Data.Effect (Newtype, Tycon)

value :: Value a -> Params a
value =
  param . nonNullable

foldable ::
  Foldable t =>
  Value a ->
  Params (t a)
foldable =
  value .
  array .
  dimension foldl' .
  element .
  nonNullable

class QueryParam (effs :: [*]) (d :: *) where
  queryParam :: Params d

instance (
    Coercible n d,
    QueryParam effs d
  ) => QueryParam (Newtype n d : effs) n where
  queryParam =
    coerce (queryParam @effs @d)

instance (
    EncoderValue effs d
  ) => QueryParam (Tycon [] d : effs) [d] where
  queryParam =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryParam (Tycon Vector d : effs) (Vector d) where
  queryParam =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryParam (Tycon NonEmpty d : effs) (NonEmpty d) where
  queryParam =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryParam (Tycon Maybe d : effs) (Maybe d) where
  queryParam =
    param (nullable (encoderValue @effs))

instance {-# overlappable #-} (
    EncoderValue effs d
  ) => QueryParam effs d where
  queryParam =
    value (encoderValue @effs)
