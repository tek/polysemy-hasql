module Polysemy.Hasql.Reader where

import Polysemy.Db.Reader (interpretReaderStore)
import Sqel.Data.QuerySchema (emptyQuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)

import Polysemy.Hasql.Effect.DbTable (StoreTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)

-- |Interpret 'Reader' as a singleton table.
--
-- Given an initial value, every state action reads the value from the database, potentially writing it on first access.
interpretReaderDb ::
  ∀ d e r .
  Member (StoreTable () d !! e) r =>
  TableSchema (Uid () d) ->
  d ->
  InterpreterFor (Reader d !! e) r
interpretReaderDb table initial =
  interpretStoreDb table emptyQuerySchema .
  interpretReaderStore initial .
  raiseUnder
{-# inline interpretReaderDb #-}
