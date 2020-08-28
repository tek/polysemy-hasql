{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Db.Store.Statement where

import Hasql.Statement (Statement)

import qualified Polysemy.Db.Data.Database as Database
import Polysemy.Db.Data.Database (Database)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Schema as Schema
import Polysemy.Db.Data.Schema (Schema(..))
import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)

class Members [Schema q d, Database d DbError] r => StatementEffects q r d where

instance Members [Schema q d, Database d DbError] r => StatementEffects q r d where

runStatement ::
  Member (Database d e) r =>
  Sem r (Statement q o) ->
  q ->
  Sem r (Either (StoreError e) o)
runStatement statementSem param = do
  statement <- statementSem
  mapLeft StoreError.Backend <$> Database.run param statement

insert ::
  StatementEffects q r d =>
  d ->
  Sem r (Either (StoreError DbError) ())
insert record =
  runStatement Schema.insert record

upsert ::
  StatementEffects q r d =>
  d ->
  Sem r (Either (StoreError DbError) ())
upsert record =
  runStatement Schema.upsert record

delete ::
  ∀ d q r .
  StatementEffects q r d =>
  q ->
  Sem r (Either (StoreError DbError) ())
delete q =
  runStatement Schema.delete q

fetch ::
  ∀ d q r .
  StatementEffects q r d =>
  q ->
  Sem r (Either (StoreError DbError) (Maybe d))
fetch q =
  runStatement Schema.fetch q

fetchAll ::
  ∀ d q r .
  StatementEffects q r d =>
  Sem r (Either (StoreError DbError) [d])
fetchAll =
  runStatement Schema.fetchAll ()
