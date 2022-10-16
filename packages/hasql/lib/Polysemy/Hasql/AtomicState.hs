module Polysemy.Hasql.AtomicState where

import Conc (interpretLockReentrant, Lock, MaskIO)
import Polysemy.Db.AtomicState (interpretAtomicStateStore)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid)

import Polysemy.Hasql.Crud (interpretCrudSingleton)
import Polysemy.Hasql.Data.Crud (UidCrud)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTableUid)
import Polysemy.Hasql.Data.Query (UidQuery)
import Polysemy.Hasql.ManagedTable (interpretManagedTableAuto)
import Polysemy.Hasql.Query (interpretQueryAuto)
import Polysemy.Hasql.Store (interpretStoreDb)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (Schema)

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an action that produces an initial value, every state action reads the value from the database and writes it
-- back.
interpretAtomicStateDb ::
  Show e =>
  BuildPartialSql d tree u =>
  Members [UidQuery () d, ManagedTableUid () d !! e, Error InitDbError, MaskIO, Resource, Race, Embed IO] r =>
  Sem (Stop e : Store () d !! e : UidCrud () d !! e : Lock @@ () : r) d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDb initial =
  interpretLockReentrant . untag .
  interpretCrudSingleton .
  interpretStoreDb .
  interpretAtomicStateStore initial .
  insertAt @1
{-# inline interpretAtomicStateDb #-}

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
interpretAtomicStateDbAs ::
  Show e =>
  BuildPartialSql d tree u =>
  Members [UidQuery () d, ManagedTableUid () d !! e, Error InitDbError, MaskIO, Resource, Race, Embed IO] r =>
  d ->
  InterpreterFor (AtomicState d !! e) r
interpretAtomicStateDbAs d =
  interpretAtomicStateDb (pure d)
{-# inline interpretAtomicStateDbAs #-}

-- |Interpret 'AtomicState' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database and writes it back.
--
-- Uses the automatic derivation strategy.
interpretAtomicStateDbAsAuto ::
  Schema Auto Auto () (Uid () d) =>
  BuildPartialSql d tree u =>
  Members [Database !! DbError, Error InitDbError, Log, MaskIO, Resource, Race, Embed IO] r =>
  d ->
  InterpreterFor (AtomicState d !! DbError) r
interpretAtomicStateDbAsAuto initial =
  interpretQueryAuto .
  interpretManagedTableAuto .
  interpretAtomicStateDbAs initial .
  raiseUnder2
{-# inline interpretAtomicStateDbAsAuto #-}
