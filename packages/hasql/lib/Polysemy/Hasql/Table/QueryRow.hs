module Polysemy.Hasql.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Constraint (ProductCoded)
import Polysemy.Hasql.Table.Representation (ProdColumn)
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

class NullVariants reps (ds :: [*]) where
  readNulls2 :: Row ()

instance NullVariants rep '[] where
  readNulls2 =
    unit

-- doing this with 'hcpure' seems to send ghc spinning because of the necessity of the constraint being @QueryRow d@
-- instead of @QueryRow (Maybe d)@
instance (
    NullVariants (ProdColumn reps) ds,
    QueryRow (Maybe d)
  ) => NullVariants (ProdColumn (rep : reps)) (Maybe d : ds) where
  readNulls2 =
    void (queryRow @(Maybe d)) *> readNulls2 @(ProdColumn reps) @ds

instance {-# overlappable #-} (
    NullVariants (ProdColumn reps) ds,
    QueryRow (Maybe d)
  ) => NullVariants (ProdColumn (rep : reps)) (d : ds) where
  readNulls2 =
    void (queryRow @(Maybe d)) *> readNulls2 @(ProdColumn reps) @ds

instance (
    ProductCoded d dSub,
    NullVariants rSub dSub,
    NullVariants reps ds
  ) => NullVariants (ProdColumn (Flatten rSub : reps)) (d : ds) where
  readNulls2 =
    readNulls2 @rSub @dSub *> readNulls2 @reps @ds
