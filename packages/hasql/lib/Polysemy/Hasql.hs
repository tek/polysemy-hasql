module Polysemy.Hasql (
  -- * Introduction
  -- $intro

  -- * Hasql Interpreters
  -- $interpreters

  interpretQuery,
  interpretQueryDd,
  mapQuery,

  -- ** Low-Level Interpreters
  Database,
  DbTable,
  StoreTable,
  DbConfig(DbConfig),

  -- * Misc
  DbConnectionError,
  interpretAtomicStateDb,
  interpretReaderDb,
) where

import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)

import Polysemy.Hasql.AtomicState (interpretAtomicStateDb)
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)
import Polysemy.Hasql.Interpreter.Query (interpretQuery, interpretQueryDd, mapQuery)
import Polysemy.Hasql.Reader (interpretReaderDb)

-- $intro
-- This library provides two independent features as well as their synthesis:
--
--   - Hasql-specific interpreters for the effects in "Polysemy.Db"
--   - A generic derivation mechanism for Hasql codecs and statements

-- $interpreters
-- Four auxiliary effects can be combined to build an interpreter stack for 'Store' and 'Query'.
-- The minimal set consists of 'DbConnection', which manages a 'Connection' value, and 'Database', which executes
-- statements, coordinates retries on broken connections, and handles initialization of tables and types.
--
-- /Note/: Effect constructors should idiomatically be used as qualified names, which is why they aren't exported here:
--
-- @
-- import qualified Polysemy.Hasql.Effect.Database as Database
--
-- prog = do
--   Database.runStatement p s
-- @

-- $derived
-- The generic derivation machinery described further down can be used with two additional effects whose interpreters
-- use auto-generated SQL statements.
-- When using any of these, the derivation can either be guided for granular configuration of columns or use defaults.
--
-- These two effects are used in conjunction with a set of interpreters for 'Polysemy.Db.Data.Store', the most
-- complete of which is 'interpretStoreDbFullGen':

-- $derived2
-- The data structure produced by the derivation is of type 'QueryTable', which can also be constructed manually:

-- $derivation
-- 'QueryTable' is derived by specifying an auxiliary datatype, called /representation/, to the class 'GenQueryTable'.
-- The special type 'Auto' indicates that everything should use default encodings.
--
-- Aside from configuring column options, the derivation system can also convert structural information, as in nested
-- data types, into columns.
