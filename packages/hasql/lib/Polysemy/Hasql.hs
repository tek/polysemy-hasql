module Polysemy.Hasql (
  -- * Introduction
  -- $intro

  -- * Hasql interpreters for [Polysemy.Db]("Polysemy.Db")
  interpretQStoreDb,
  interpretQStoreXa,
  interpretQStores,
  interpretStoreDb,
  interpretStoreXa,
  interpretStores,

  interpretQuery,
  interpretQueryWith,
  interpretQueryStatement,
  interpretQueryStatementWith,
  interpretQueryProj,
  interpretQueryProjWith,

  -- * Database effects
  DbConnectionPool,
  Database,
  Databases,
  DbTable,
  withTable,
  statement,
  StoreTable,
  Transaction,
  Transactions,
  abort,

  -- * Database interpeters
  interpretDbConnectionPool,
  interpretDbConnectionPoolSingle,

  interpretDatabase,
  interpretDatabases,
  interpretHasql,

  interpretTablesMigrations,
  interpretTableMigrations,
  interpretTableMigrationsScoped,
  interpretTables,
  interpretTable,

  -- * Misc combinators
  queryVia,
  mapQuery,

  -- * Misc
  interpretAtomicStateDb,
  interpretAtomicStatesDb,
  interpretReaderDb,
) where

import Polysemy.Hasql.Effect.Database (Database, Databases)
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable, statement, withTable)
import Polysemy.Hasql.Effect.Transaction (Transaction, Transactions, abort)
import Polysemy.Hasql.Interpreter.AtomicState (interpretAtomicStateDb, interpretAtomicStatesDb)
import Polysemy.Hasql.Interpreter.Database (interpretDatabase, interpretDatabases, interpretHasql)
import Polysemy.Hasql.Interpreter.DbConnectionPool (interpretDbConnectionPool, interpretDbConnectionPoolSingle)
import Polysemy.Hasql.Interpreter.DbTable (
  interpretTable,
  interpretTableMigrations,
  interpretTableMigrationsScoped,
  interpretTables,
  interpretTablesMigrations,
  )
import Polysemy.Hasql.Interpreter.Query (
  interpretQuery,
  interpretQueryProj,
  interpretQueryProjWith,
  interpretQueryStatement,
  interpretQueryStatementWith,
  interpretQueryWith,
  mapQuery,
  queryVia,
  )
import Polysemy.Hasql.Interpreter.Reader (interpretReaderDb)
import Polysemy.Hasql.Interpreter.Store (
  interpretQStoreDb,
  interpretQStoreXa,
  interpretQStores,
  interpretStoreDb,
  interpretStoreXa,
  interpretStores,
  )

-- $intro
-- This library provides Hasql-specific interpreters for the effects in [Polysemy.Db]("Polysemy.Db")
