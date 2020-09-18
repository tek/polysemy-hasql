module Polysemy.Hasql.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)

import Polysemy.Hasql.Table.ValueDecoder (ValueDecoder(..))

class QueryRow a where
  queryRow :: Row a

value :: Value a -> Row a
value =
  column . nonNullable

values :: Array a -> Row a
values =
  value . array

instance ValueDecoder a => QueryRow [a] where
  queryRow =
    value (listArray (nonNullable valueDecoder))
  {-# inline queryRow #-}

instance ValueDecoder a => QueryRow (Vector a) where
  queryRow =
    value (vectorArray (nonNullable valueDecoder))
  {-# inline queryRow #-}

instance ValueDecoder a => QueryRow (NonEmpty a) where
  queryRow = do
    result <- nonEmpty <$> value (listArray (nonNullable valueDecoder))
    maybe (fail "no elements in NonEmpty field") pure result
  {-# inline queryRow #-}

instance ValueDecoder a => QueryRow (Maybe a) where
  queryRow =
    column (nullable valueDecoder)
  {-# inline queryRow #-}

instance {-# overlappable #-} ValueDecoder a => QueryRow a where
  queryRow =
    value valueDecoder
  {-# inline queryRow #-}
