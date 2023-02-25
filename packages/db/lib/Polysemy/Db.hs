module Polysemy.Db (
  -- $intro
  module Polysemy.Db.Effect.Store,
  module Polysemy.Db.Effect.Query,
  module Polysemy.Db.Store,
  interpretAtomicStateStore,
  DbError,
  InitDbError,
) where

import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Effect.Query (Query, query)
import Polysemy.Db.Effect.Store (QStore, Store, delete, deleteAll, fetch, fetchAll, insert, upsert)
import Polysemy.Db.Store

-- $intro
-- The 'Polysemy' effects 'Store' and 'Query' provide a high-level abstraction of database operations for
-- CRUD and arbitrarily complex queries.
--
