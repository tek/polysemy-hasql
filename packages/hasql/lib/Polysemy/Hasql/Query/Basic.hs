module Polysemy.Hasql.Query.Basic where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.Partial (Partial, getPartial)
import Polysemy.Db.Data.StoreQuery (StoreQuery (Basic))

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)

interpretStoreQueryWith ::
  ∀ f qOut qIn dIn dOut dResult e r .
  Functor f =>
  Member (ManagedTable dIn !! e) r =>
  Statement qIn (f dIn) ->
  (f dOut -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut dResult !! e) r
interpretStoreQueryWith statement result fromQ toD =
  interpretResumable \case
    Basic params ->
      result . fmap toD <$> restop (ManagedTable.runStatement (fromQ params) statement)

interpretStoreQueryUsing ::
  ∀ f qOut qIn dIn dOut dResult e r .
  Show e =>
  Functor f =>
  Members [Query qIn dIn, ManagedTable dIn !! e, Error InitDbError] r =>
  (QueryTable qIn dIn -> Statement qIn (f dIn)) ->
  (f dOut -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut dResult !! e) r
interpretStoreQueryUsing create result fromQ toD sem = do
  statement <- create <$> queryTable
  interpretStoreQueryWith statement result fromQ toD sem

interpretStoreQuery ::
  ∀ q d a e r .
  Member (ManagedTable d !! e) r =>
  Statement q a ->
  InterpreterFor (StoreQuery q a !! e) r
interpretStoreQuery statement =
  interpretResumable \case
    Basic params ->
      restop (ManagedTable.runStatement params statement)

interpretStoreQueryPartialUpdate ::
  ∀ i d e r u tree .
  Show e =>
  BuildPartialSql d tree u =>
  Members [Query i d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (StoreQuery (i, Partial d) (Maybe d) !! e) r
interpretStoreQueryPartialUpdate =
  interpretResumable \case
    Basic (i, patch) -> do
      table <- queryTable
      restop (ManagedTable.runStatement () (Statement.update table i (getPartial patch)))
