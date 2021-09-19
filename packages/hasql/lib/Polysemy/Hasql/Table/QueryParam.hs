module Polysemy.Hasql.Table.QueryParam where

import Data.Vector (Vector)
import Hasql.Encoders (NullableOrNot, Params, Value, array, dimension, element, noParams, nonNullable, nullable, param)
import Polysemy.Db.Data.Rep (Prim)
import Polysemy.Db.Tree.Data.Effect (Newtype, Tycon)

import Polysemy.Hasql.Table.EncoderValue (EncoderValue, encoderValue)

value :: Value a -> Params a
value =
  param . nonNullable

foldable ::
  Foldable t =>
  Value a ->
  NullableOrNot Value (t a)
foldable =
  nonNullable .
  array .
  dimension foldl' .
  element .
  nonNullable

class QueryValueNoN (effs :: [Type]) (d :: Type) where
  queryValueNoN :: NullableOrNot Value d

instance (
    EncoderValue effs d
  ) => QueryValueNoN (Tycon [] d : effs) [d] where
  queryValueNoN =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryValueNoN (Tycon Vector d : effs) (Vector d) where
  queryValueNoN =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryValueNoN (Tycon NonEmpty d : effs) (NonEmpty d) where
  queryValueNoN =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryValueNoN (Tycon Set d : effs) (Set d) where
  queryValueNoN =
    foldable (encoderValue @effs)

instance (
    EncoderValue effs d
  ) => QueryValueNoN (Tycon Maybe d : effs) (Maybe d) where
  queryValueNoN =
    nullable (encoderValue @effs)

instance {-# overlappable #-} (
    EncoderValue effs d
  ) => QueryValueNoN effs d where
  queryValueNoN =
    nonNullable (encoderValue @effs)

class QueryParam (effs :: [Type]) (d :: Type) where
  queryParam :: Params d

instance {-# overlappable #-} (
    QueryValueNoN effs d
  ) => QueryParam effs d where
    queryParam =
      param (queryValueNoN @effs)

instance (
    Coercible n d,
    QueryParam effs d
  ) => QueryParam (Newtype n d : effs) n where
  queryParam =
    coerce (queryParam @effs @d)

instance QueryParam '[Prim] () where
  queryParam =
    noParams
