module Polysemy.Hasql.AtomicState where

import Conc (interpretLockReentrant)
import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Sqel.Data.QuerySchema (emptyQuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)

import Polysemy.Hasql.Effect.DbTable (StoreTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every state action reads the value from the database and writes it
-- back.
interpretAtomicStateDb ::
  Members [StoreTable () d !! e, Error InitDbError, Mask, Resource, Race, Embed IO] r =>
  TableSchema (Uid () d) ->
  Sem r d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDb table initial =
  interpretLockReentrant . untag .
  interpretStoreDb table emptyQuerySchema .
  interpretAtomicStateStore (insertAt @0 initial) .
  insertAt @1
{-# inline interpretAtomicStateDb #-}
