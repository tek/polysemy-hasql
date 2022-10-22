{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Store.Statement where

import Hasql.Statement (Statement)

import qualified Polysemy.Hasql.Data.Crud as Crud
import Polysemy.Hasql.Data.Crud (Crud (..))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table.ResultShape (ResultShape)

class Members [Crud i d q !! e, ManagedTable d !! e, Stop e] r => StatementEffects i e r d q

instance Members [Crud i d q !! e, ManagedTable d !! e, Stop e] r => StatementEffects i e r d q

runStatement ::
  Members [ManagedTable d !! e, Stop e] r =>
  Sem r (Statement i o) ->
  i ->
  Sem r o
runStatement statementSem param =
  restop . ManagedTable.runStatement param =<< statementSem

insert ::
  StatementEffects i e r d q =>
  d ->
  Sem r ()
insert =
  runStatement (restop Crud.insert)

upsert ::
  StatementEffects i e r d q =>
  d ->
  Sem r ()
upsert =
  runStatement (restop Crud.upsert)

delete ::
  ∀ i d q e r .
  StatementEffects i e r d q =>
  i ->
  Sem r [d]
delete =
  runStatement (restop Crud.delete)

deleteAll ::
  ∀ i d q e r .
  StatementEffects i e r d q =>
  Sem r [d]
deleteAll =
  runStatement (restop Crud.deleteAll) ()

fetch ::
  ∀ i d o q e r .
  ResultShape d o =>
  StatementEffects i e r d q =>
  i ->
  Sem r o
fetch =
  runStatement (restop Crud.fetch)

fetchQ ::
  ∀ i d o q e r .
  ResultShape d o =>
  StatementEffects i e r d q =>
  q ->
  Sem r o
fetchQ =
  runStatement (restop Crud.fetchQ)

fetchAll ::
  ∀ i d q e r .
  StatementEffects i e r d q =>
  Sem r [d]
fetchAll =
  runStatement (restop Crud.fetchAll) ()
