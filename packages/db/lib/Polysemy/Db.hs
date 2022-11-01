module Polysemy.Db (
  -- $intro
  module Polysemy.Db.Effect.Store,
  module Polysemy.Db.Effect.StoreQuery,
  module Polysemy.Db.Store,
  interpretAtomicStateStore,
) where

import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Effect.Store (Store, delete, deleteAll, fetch, fetchAll, insert, upsert)
import Polysemy.Db.Effect.StoreQuery (StoreQuery, basic)
import Polysemy.Db.Store (interpretStoreAtomic)

-- $intro
-- The 'Polysemy' effects 'Store' and 'StoreQuery' provide a high-level abstraction of database operations for
-- CRUD and arbitrarily complex queries.
--
