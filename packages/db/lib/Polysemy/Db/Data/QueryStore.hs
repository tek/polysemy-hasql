module Polysemy.Db.Data.QueryStore where

import Polysemy.Db.Data.Partial (Partial)

-- |A 'QueryStore' is more flexible version of 'Store', allowing an additional query type and a custom record type that
-- differs from the primary key.
data QueryStore i d q p :: Effect where
  Insert :: d -> QueryStore i d q p m ()
  Upsert :: d -> QueryStore i d q p m ()
  Delete :: i -> QueryStore i d q p m (Maybe (NonEmpty d))
  DeleteAll :: QueryStore i d q p m (Maybe (NonEmpty d))
  Fetch :: i -> QueryStore i d q p m (Maybe d)
  FetchAll :: QueryStore i d q p m (Maybe (NonEmpty d))
  Query :: q -> QueryStore i d q p m (Maybe (NonEmpty d))
  Update :: i -> Partial p -> QueryStore i d q p m (Maybe d)
  UpdateQuery :: q -> Partial p -> QueryStore i d q p m (Maybe d)

makeSem ''QueryStore

type UuidQueryStore d q p =
  QueryStore UUID d q p
