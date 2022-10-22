module Polysemy.Db.Data.QueryStore where

import Data.UUID (UUID)

-- |A 'QueryStore' is more flexible version of 'Store', allowing an additional query type and a custom record type that
-- differs from the primary key.
data QueryStore i d q :: Effect where
  Insert :: d -> QueryStore i d q m ()
  Upsert :: d -> QueryStore i d q m ()
  Delete :: i -> QueryStore i d q m (Maybe (NonEmpty d))
  DeleteAll :: QueryStore i d q m (Maybe (NonEmpty d))
  Fetch :: i -> QueryStore i d q m (Maybe d)
  FetchAll :: QueryStore i d q m (Maybe (NonEmpty d))
  Query :: q -> QueryStore i d q m (Maybe (NonEmpty d))

makeSem ''QueryStore

type UuidQueryStore d q =
  QueryStore UUID d q
