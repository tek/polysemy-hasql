{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Store.Statement where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.PartialFields (PartialFields)
-- import Polysemy.Db.Partial.Store (UpdateStorePartial)

import qualified Polysemy.Hasql.Data.Crud as Crud
import Polysemy.Hasql.Data.Crud (Crud(..))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)

class Members [Crud q d !! e, ManagedTable d !! e, Stop e] r => StatementEffects q e r d

instance Members [Crud q d !! e, ManagedTable d !! e, Stop e] r => StatementEffects q e r d

runStatement ::
  Members [ManagedTable d !! e, Stop e] r =>
  Sem r (Statement q o) ->
  q ->
  Sem r o
runStatement statementSem param =
  restop . ManagedTable.runStatement param =<< statementSem

insert ::
  StatementEffects q e r d =>
  d ->
  Sem r ()
insert =
  runStatement (restop Crud.insert)

upsert ::
  StatementEffects q e r d =>
  d ->
  Sem r ()
upsert =
  runStatement (restop Crud.upsert)

delete ::
  ∀ d q e r .
  StatementEffects q e r d =>
  q ->
  Sem r [d]
delete =
  runStatement (restop Crud.delete)

deleteAll ::
  ∀ d q e r .
  StatementEffects q e r d =>
  Sem r [d]
deleteAll =
  runStatement (restop Crud.deleteAll) ()

fetch ::
  ∀ d q e r .
  StatementEffects q e r d =>
  q ->
  Sem r (Maybe d)
fetch q =
  runStatement (restop Crud.fetch) q

fetchAll ::
  ∀ d q e r .
  StatementEffects q e r d =>
  Sem r [d]
fetchAll =
  runStatement (restop Crud.fetchAll) ()

update ::
  -- ∀ d q u e r .
  -- StatementEffects q e r d =>
  q ->
  PartialFields u ->
  Sem r ()
update _ _ =
  undefined
  -- runStatement (restop Crud.update) (q, u)
