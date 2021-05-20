module Polysemy.Hasql.AtomicState where

import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Log (Log)

import Polysemy.Hasql.Crud (interpretCrudSingleton)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.ManagedTable (interpretManagedTableAuto)
import Polysemy.Hasql.Query (interpretQueryAuto)
import Polysemy.Hasql.Store (interpretStoreDb)
import Polysemy.Hasql.Table.Schema (Schema)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
interpretAtomicStateDb ::
  Show e =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDb initial =
  interpretCrudSingleton .
  interpretStoreDb .
  interpretAtomicStateStore initial .
  raiseUnder2
{-# INLINE interpretAtomicStateDb #-}

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
--
-- Uses the automatic derivation strategy.
interpretAtomicStateDbAuto ::
  Schema Auto Auto () d =>
  Members [Database !! DbError, Error InitDbError, Log, Embed IO] r =>
  d ->
  InterpreterFor (AtomicState d !! DbError) r
interpretAtomicStateDbAuto initial =
  interpretQueryAuto .
  interpretManagedTableAuto .
  interpretAtomicStateDb initial .
  raiseUnder2
{-# INLINE interpretAtomicStateDbAuto #-}
