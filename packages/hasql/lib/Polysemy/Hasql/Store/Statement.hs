{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Store.Statement where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.Partial (Partial)

import qualified Polysemy.Hasql.Data.Crud as Crud
import Polysemy.Hasql.Data.Crud (Crud (..))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table.ResultShape (ResultShape)

class Members [Crud i d q p !! e, ManagedTable d !! e, Stop e] r => StatementEffects i e r d q p

instance Members [Crud i d q p !! e, ManagedTable d !! e, Stop e] r => StatementEffects i e r d q p

runStatement ::
  Members [ManagedTable d !! e, Stop e] r =>
  Sem r (Statement i o) ->
  i ->
  Sem r o
runStatement statementSem param =
  restop . ManagedTable.runStatement param =<< statementSem

insert ::
  StatementEffects i e r d q p =>
  d ->
  Sem r ()
insert =
  runStatement (restop Crud.insert)

upsert ::
  StatementEffects i e r d q p =>
  d ->
  Sem r ()
upsert =
  runStatement (restop Crud.upsert)

delete ::
  ∀ i d q p e r .
  StatementEffects i e r d q p =>
  i ->
  Sem r [d]
delete =
  runStatement (restop Crud.delete)

deleteAll ::
  ∀ i d q p e r .
  StatementEffects i e r d q p =>
  Sem r [d]
deleteAll =
  runStatement (restop Crud.deleteAll) ()

fetch ::
  ∀ i d o q p e r .
  ResultShape d o =>
  StatementEffects i e r d q p =>
  i ->
  Sem r o
fetch =
  runStatement (restop Crud.fetch)

fetchQ ::
  ∀ i d o q p e r .
  ResultShape d o =>
  StatementEffects i e r d q p =>
  q ->
  Sem r o
fetchQ =
  runStatement (restop Crud.fetchQ)

fetchAll ::
  ∀ i d q p e r .
  StatementEffects i e r d q p =>
  Sem r [d]
fetchAll =
  runStatement (restop Crud.fetchAll) ()

update ::
  StatementEffects i e r d q p =>
  i ->
  Partial p ->
  Sem r (Maybe d)
update i patch =
  runStatement (restop (Crud.update i patch)) ()

updateQ ::
  StatementEffects i e r d q p =>
  q ->
  Partial p ->
  Sem r (Maybe d)
updateQ q patch =
  runStatement (restop (Crud.updateQ q patch)) ()
