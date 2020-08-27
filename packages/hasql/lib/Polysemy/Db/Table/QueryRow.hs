module Polysemy.Db.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)

import Polysemy.Db.Table.ValueDecoder (ValueDecoder(..))

class QueryRow a where
  queryRow :: Row a

value :: Value a -> Row a
value =
  column . nonNullable

values :: Array a -> Row a
values =
  value . array

instance ValueDecoder a => QueryRow [a] where
  {-# INLINE queryRow #-}
  queryRow =
    value (listArray (nonNullable valueDecoder))

instance ValueDecoder a => QueryRow (Vector a) where
  {-# INLINE queryRow #-}
  queryRow =
    value (vectorArray (nonNullable valueDecoder))

instance ValueDecoder a => QueryRow (NonEmpty a) where
  {-# INLINE queryRow #-}
  queryRow = do
    result <- nonEmpty <$> value (listArray (nonNullable valueDecoder))
    maybe (fail "no elements in NonEmpty field") pure result

instance ValueDecoder a => QueryRow (Maybe a) where
  {-# INLINE queryRow #-}
  queryRow =
    column (nullable valueDecoder)

instance {-# OVERLAPPABLE #-} ValueDecoder a => QueryRow a where
  {-# INLINE queryRow #-}
  queryRow =
    value valueDecoder
