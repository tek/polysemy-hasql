module Polysemy.Hasql.AtomicState where

import Conc (interpretLockReentrant)
import Hasql.Connection (Connection)
import Polysemy.Db.AtomicState (interpretAtomicStateStore, interpretAtomicStatesStore)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Sqel.Data.QuerySchema (emptyQuerySchema)
import Sqel.Data.TableSchema (TableSchema)

import Polysemy.Hasql.Effect.Database (ConnectionSource)
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Store (interpretQStoreDb, interpretQStores)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every state action reads the value from the database and writes it
-- back.
interpretAtomicStateDb ::
  Members [DbTable d !! e, Error InitDbError, Mask, Resource, Race, Embed IO] r =>
  TableSchema d ->
  Sem r d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDb table initial =
  interpretLockReentrant . untag .
  interpretQStoreDb @Maybe table emptyQuerySchema .
  interpretAtomicStateStore (insertAt @0 initial) .
  insertAt @1

interpretAtomicStatesDb ::
  Members [Error InitDbError, Mask, Resource, Race, Embed IO] r =>
  Members [Scoped ConnectionSource (DbTable d !! err), DbTable d !! err, Log, Embed IO] r =>
  TableSchema d ->
  Sem r d ->
  InterpretersFor [AtomicState d !! err, Scoped Connection (AtomicState d !! err) !! err] r
interpretAtomicStatesDb table initial =
  interpretLockReentrant . untag .
  interpretQStores @Maybe table emptyQuerySchema .
  interpretAtomicStatesStore (insertAt @0 initial) .
  insertAt @2
