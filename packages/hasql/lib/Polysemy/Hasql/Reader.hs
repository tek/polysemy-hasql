module Polysemy.Hasql.Reader where

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Reader (interpretReaderStore)
import Polysemy.Log (Log)

import Polysemy.Hasql.Crud (interpretCrudSingleton)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.ManagedTable (interpretManagedTableAuto)
import Polysemy.Hasql.Query (interpretQueryAuto)
import Polysemy.Hasql.Store (interpretStoreDb)
import Polysemy.Hasql.Table.Schema (Schema)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
interpretReaderDb ::
  ∀ d e r .
  Show e =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  d ->
  InterpreterFor (Reader d !! e) r
interpretReaderDb initial =
  interpretCrudSingleton .
  interpretStoreDb .
  interpretReaderStore initial .
  raiseUnder2
{-# INLINE interpretReaderDb #-}

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
--
-- Uses the automatic derivation strategy.
interpretReaderDbAuto ::
  ∀ d r .
  Schema Auto Auto () d =>
  Members [Database !! DbError, Error InitDbError, Log, Embed IO] r =>
  d ->
  InterpreterFor (Reader d !! DbError) r
interpretReaderDbAuto initial =
  interpretQueryAuto .
  interpretManagedTableAuto .
  interpretReaderDb initial .
  raiseUnder2
{-# INLINE interpretReaderDbAuto #-}
