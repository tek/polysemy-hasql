module Polysemy.Db (
  -- $intro
  module Polysemy.Db.Data.Store,
  module Polysemy.Db.Data.StoreQuery,
  module Polysemy.Db.Store,
  interpretAtomicStateStore,
) where

import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.Store (Store, delete, deleteAll, fetch, fetchAll, insert, upsert)
import Polysemy.Db.Data.StoreQuery (StoreQuery, basic)
import Polysemy.Db.Store (interpretStoreAtomic)

-- $intro
-- The 'Polysemy' effects 'Store' and 'StoreQuery' provide a high-level abstraction of database operations for
-- CRUD and arbitrarily complex queries.
--
