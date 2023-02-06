module Polysemy.Hasql.Reader where

import Polysemy.Db.Reader (interpretReaderStore)
import Sqel.Data.QuerySchema (emptyQuerySchema)
import Sqel.Data.TableSchema (TableSchema)

import Polysemy.Hasql.Effect.Database (ConnectionSource)
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Store (interpretQStoreDb)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
interpretReaderDb ::
  ∀ d e r .
  Member (Scoped ConnectionSource (DbTable d !! e)) r =>
  TableSchema d ->
  Sem r d ->
  InterpreterFor (Reader d !! e) r
interpretReaderDb table initial =
  interpretQStoreDb @Maybe table emptyQuerySchema .
  interpretReaderStore (raise initial) .
  raiseUnder
