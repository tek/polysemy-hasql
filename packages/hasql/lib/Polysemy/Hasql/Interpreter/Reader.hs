module Polysemy.Hasql.Interpreter.Reader where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Interpreter.Reader (interpretReaderStore)
import Sqel (DdType, Sqel, emptyQuery)

import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Interpreter.Store (interpretQStoreDb)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
interpretReaderDb ::
  ∀ table r .
  Member (DbTable (DdType table) !! DbError) r =>
  Sqel table ->
  Sem r (DdType table) ->
  InterpreterFor (Reader (DdType table) !! DbError) r
interpretReaderDb table initial =
  interpretQStoreDb @Maybe emptyQuery table .
  interpretReaderStore (raise initial) .
  raiseUnder
