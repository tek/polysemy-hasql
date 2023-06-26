module Polysemy.Hasql.Interpreter.AtomicState where

import Conc (interpretLockReentrant)
import Hasql.Connection (Connection)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Interpreter.AtomicState (interpretAtomicStateStore, interpretAtomicStatesStore)
import Sqel (DdType, Sqel, emptyQuery)

import Polysemy.Hasql.Effect.Database (ConnectionSource)
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Store (interpretQStoreDb, interpretQStores)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every state action reads the value from the database and writes it
-- back.
interpretAtomicStateDb ::
  Members [DbTable (DdType table) !! DbError, Error InitDbError, Mask, Resource, Race, Embed IO] r =>
  Sqel table ->
  Sem r (DdType table) ->
  InterpreterFor (AtomicState (DdType table) !! DbError) r
interpretAtomicStateDb table initial =
  interpretLockReentrant . untag .
  interpretQStoreDb @Maybe emptyQuery table .
  interpretAtomicStateStore (insertAt @0 initial) .
  insertAt @1

interpretAtomicStatesDb ::
  Members [Error InitDbError, Mask, Resource, Race, Embed IO] r =>
  Members [Scoped ConnectionSource (DbTable (DdType table) !! DbError), DbTable (DdType table) !! DbError, Log, Embed IO] r =>
  Sqel table ->
  Sem r (DdType table) ->
  InterpretersFor [AtomicState (DdType table) !! DbError, Scoped Connection (AtomicState (DdType table) !! DbError) !! DbError] r
interpretAtomicStatesDb table initial =
  interpretLockReentrant . untag .
  interpretQStores @Maybe emptyQuery table .
  interpretAtomicStatesStore (insertAt @0 initial) .
  insertAt @2
