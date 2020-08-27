{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Db.Store.Statement where

import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.Database as Database
import Polysemy.Db.Data.Database (Database)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Schema as Schema
import Polysemy.Db.Data.Schema (Schema(..))

class Members [Schema q d, Database d DbError] r => StatementEffects q r d where

instance Members [Schema q d, Database d DbError] r => StatementEffects q r d where

runStatement ::
  Sem r s ->
  (s -> Sem r (Either e o)) ->
  Sem r (Either (StoreError e) o)
runStatement statementSem writer = do
  statement <- statementSem
  mapLeft StoreError.Backend <$> writer statement

insert ::
  StatementEffects q r d =>
  d ->
  Sem r (Either (StoreError DbError) ())
insert record =
  runStatement Schema.insert (Database.run record)

upsert ::
  StatementEffects q r d =>
  d ->
  Sem r (Either (StoreError DbError) ())
upsert record =
  runStatement Schema.upsert (Database.run record)

delete ::
  ∀ d q r .
  StatementEffects q r d =>
  q ->
  Sem r (Either (StoreError DbError) ())
delete q =
  runStatement Schema.delete (Database.run q)

fetch ::
  ∀ d q r .
  StatementEffects q r d =>
  q ->
  Sem r (Either (StoreError DbError) (Maybe d))
fetch q =
  runStatement Schema.fetch (Database.run q)

fetchAll ::
  ∀ d q r .
  StatementEffects q r d =>
  Sem r (Either (StoreError DbError) [d])
fetchAll =
  runStatement Schema.fetchAll (Database.run ())
