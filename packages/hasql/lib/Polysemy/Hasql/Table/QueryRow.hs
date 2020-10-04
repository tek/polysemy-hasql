module Polysemy.Hasql.Table.QueryRow where

import Data.Vector (Vector)
import Hasql.Decoders (Array, Row, Value, array, column, listArray, nonNullable, nullable, vectorArray)

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Constraint (ProductCoded)
import Polysemy.Hasql.Table.Representation (ProdColumn)
import Polysemy.Hasql.Table.ValueDecoder (RepDecoder(..))

class QueryRow (rep :: *) (a :: *) where
  queryRow :: Row a

value :: Value a -> Row a
value =
  column . nonNullable

values :: Array a -> Row a
values =
  value . array

instance RepDecoder r a => QueryRow r [a] where
  queryRow =
    value (listArray (nonNullable (repDecoder @r)))
  {-# inline queryRow #-}

instance RepDecoder r a => QueryRow r (Vector a) where
  queryRow =
    value (vectorArray (nonNullable (repDecoder @r)))
  {-# inline queryRow #-}

instance RepDecoder r a => QueryRow r (NonEmpty a) where
  queryRow = do
    result <- nonEmpty <$> value (listArray (nonNullable (repDecoder @r)))
    maybe (fail "no elements in NonEmpty field") pure result
  {-# inline queryRow #-}

instance RepDecoder r a => QueryRow r (Maybe a) where
  queryRow =
    column (nullable (repDecoder @r))
  {-# inline queryRow #-}

instance {-# overlappable #-} RepDecoder r a => QueryRow r a where
  queryRow =
    value (repDecoder @r)
  {-# inline queryRow #-}

class NullVariant reps (ds :: [*]) where
  readNulls :: Row ()

instance NullVariant rep '[] where
  readNulls =
    unit

-- doing this with 'hcpure' seems to send ghc spinning because of the necessity of the constraint being @QueryRow d@
-- instead of @QueryRow (Maybe d)@
instance (
    NullVariant (ProdColumn reps) ds,
    QueryRow rep (Maybe d)
  ) => NullVariant (ProdColumn (rep : reps)) (Maybe d : ds) where
  readNulls =
    void (queryRow @rep @(Maybe d)) *> readNulls @(ProdColumn reps) @ds

instance {-# overlappable #-} (
    NullVariant (ProdColumn reps) ds,
    QueryRow rep (Maybe d)
  ) => NullVariant (ProdColumn (rep : reps)) (d : ds) where
  readNulls =
    void (queryRow @rep @(Maybe d)) *> readNulls @(ProdColumn reps) @ds

instance (
    ProductCoded d dSub,
    NullVariant rSub dSub,
    NullVariant reps ds
  ) => NullVariant (ProdColumn (Flatten rSub : reps)) (d : ds) where
  readNulls =
    readNulls @rSub @dSub *> readNulls @reps @ds

class NullVariants rep (dss :: [[*]]) where
  readNulls2 :: Row ()

instance NullVariants reps '[] where
  readNulls2 =
    unit

instance (
    NullVariant rep ds,
    NullVariants reps dss
  ) => NullVariants (rep : reps) (ds : dss) where
  readNulls2 =
    readNulls @rep @ds *> readNulls2 @reps @dss
