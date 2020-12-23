module Polysemy.Hasql.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)

import Polysemy.Hasql.Table.DecoderValue (DecoderValue, decoderValue)
import Polysemy.Hasql.Column.Data.Effect (Newtype, Tc)

value :: Value a -> Row a
value =
  column . nonNullable

values :: Array a -> Row a
values =
  value . array

class QueryRow (eff :: [*]) (d :: *) where
  queryRow :: Row d

instance (
    Coercible n d,
    QueryRow eff d
  ) => QueryRow (Newtype n d : eff) n where
  queryRow =
    coerce <$> queryRow @eff @d

instance DecoderValue eff d => QueryRow (Tc [] d : eff) [d] where
  queryRow =
    value (listArray (nonNullable (decoderValue @eff)))

instance DecoderValue eff d => QueryRow (Tc Vector d : eff) (Vector d) where
  queryRow =
    value (vectorArray (nonNullable (decoderValue @eff)))

instance DecoderValue eff d => QueryRow (Tc NonEmpty d : eff) (NonEmpty d) where
  queryRow = do
    result <- nonEmpty <$> value (listArray (nonNullable (decoderValue @eff)))
    maybe (fail "no elements in NonEmpty field") pure result

instance DecoderValue eff d => QueryRow (Tc Maybe d : eff) (Maybe d) where
  queryRow =
    column (nullable (decoderValue @eff))

instance {-# overlappable #-} (
    DecoderValue eff d
  ) => QueryRow eff d where
  queryRow =
    value (decoderValue @eff)
