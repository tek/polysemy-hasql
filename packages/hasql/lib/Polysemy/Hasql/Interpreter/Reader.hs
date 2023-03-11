module Polysemy.Hasql.Interpreter.Reader where

import Polysemy.Db.Interpreter.Reader (interpretReaderStore)
import Sqel (emptyQuerySchema)
import Sqel (TableSchema)

import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Store (interpretQStoreDb)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
interpretReaderDb ::
  ∀ d e r .
  Member (DbTable d !! e) r =>
  TableSchema d ->
  Sem r d ->
  InterpreterFor (Reader d !! e) r
interpretReaderDb table initial =
  interpretQStoreDb @Maybe table emptyQuerySchema .
  interpretReaderStore (raise initial) .
  raiseUnder
