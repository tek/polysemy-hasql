module Polysemy.Hasql.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)
import Polysemy.Db.Data.Rep (Prim)
import Polysemy.Db.Tree.Data.Effect (Newtype, Tycon)

import Polysemy.Hasql.Table.DecoderValue (DecoderValue, decoderValue)

value :: Value a -> Row a
value =
  column . nonNullable

values :: Array a -> Row a
values =
  value . array

class QueryRow (eff :: [Type]) (d :: Type) where
  queryRow :: Row d

instance (
    Coercible n d,
    QueryRow eff d
  ) => QueryRow (Newtype n d : eff) n where
  queryRow =
    coerce <$> queryRow @eff @d

instance DecoderValue eff d => QueryRow (Tycon [] d : eff) [d] where
  queryRow =
    value (listArray (nonNullable (decoderValue @eff)))

instance DecoderValue eff d => QueryRow (Tycon Vector d : eff) (Vector d) where
  queryRow =
    value (vectorArray (nonNullable (decoderValue @eff)))

instance DecoderValue eff d => QueryRow (Tycon NonEmpty d : eff) (NonEmpty d) where
  queryRow = do
    result <- nonEmpty <$> value (listArray (nonNullable (decoderValue @eff)))
    maybe (fail "no elements in NonEmpty field") pure result

instance DecoderValue eff d => QueryRow (Tycon Maybe d : eff) (Maybe d) where
  queryRow =
    column (nullable (decoderValue @eff))

instance {-# overlappable #-} (
    DecoderValue eff d
  ) => QueryRow eff d where
  queryRow =
    value (decoderValue @eff)

instance QueryRow '[Prim] () where
  queryRow =
    unit
