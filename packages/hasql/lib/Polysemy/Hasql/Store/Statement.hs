{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Store.Statement where

import Hasql.Statement (Statement)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.Schema as Schema
import Polysemy.Hasql.Data.Schema (Schema(..))

class Members [Schema q d ! e, ManagedTable d ! e, Stop e] r => StatementEffects q e r d where

instance Members [Schema q d ! e, ManagedTable d ! e, Stop e] r => StatementEffects q e r d where

runStatement ::
  Members [ManagedTable d ! e, Stop e] r =>
  Sem r (Statement q o) ->
  q ->
  Sem r o
runStatement statementSem param =
  restop . ManagedTable.runStatement param =<< statementSem

insert ::
  StatementEffects q e r d =>
  d ->
  Sem r ()
insert record =
  runStatement (restop Schema.insert) record

upsert ::
  StatementEffects q e r d =>
  d ->
  Sem r ()
upsert record =
  runStatement (restop Schema.upsert) record

delete ::
  ∀ d q e r .
  StatementEffects q e r d =>
  q ->
  Sem r [d]
delete q =
  runStatement (restop Schema.delete) q

fetch ::
  ∀ d q e r .
  StatementEffects q e r d =>
  q ->
  Sem r (Maybe d)
fetch q =
  runStatement (restop Schema.fetch) q

fetchAll ::
  ∀ d q e r .
  StatementEffects q e r d =>
  Sem r [d]
fetchAll =
  runStatement (restop Schema.fetchAll) ()
