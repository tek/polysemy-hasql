module Polysemy.Hasql.Interpreter.Transaction where

import Conc (interpretScopedResumableWithH)
import Hasql.Connection (Connection)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (ConnectionSource, Database, withDatabaseUnique)
import Polysemy.Hasql.Effect.Transaction (Transaction (Abort, Resource), Transactions)
import Polysemy.Hasql.Statement.Transaction (beginTransaction, commitTransaction, rollbackTransaction)

transactionScopeWithConnection ::
  Members [Database, Resource, Stop DbError] r =>
  (Connection -> Sem (Stop DbError : r) a) ->
  Sem r a
transactionScopeWithConnection use =
  bracketOnError (runS (beginTransaction def False)) abortTr \ () -> do
    Database.use \ connection ->
      subsume (use connection) <* runS (commitTransaction False)
  where
    abortTr _ = runS (rollbackTransaction False)
    runS = Database.statement ()

transactionScope ::
  Members [Scoped ConnectionSource (Database !! DbError), Resource] (Stop DbError : r) =>
  (Connection -> Sem (Stop DbError : r) a) ->
  Sem (Stop DbError : r) a
transactionScope use =
  withDatabaseUnique Nothing $ restop @DbError @Database do
    transactionScopeWithConnection (insertAt @1 . use)

interpretTransactions ::
  Members [Scoped ConnectionSource (Database !! DbError), Resource] r =>
  InterpreterFor (Transactions Connection !! DbError) r
interpretTransactions =
  interpretScopedResumableWithH @'[] (const transactionScope) \ conn -> \case
    Resource ->
      pureT conn
    Abort ->
      stop (DbError.Query "aborted by user")
