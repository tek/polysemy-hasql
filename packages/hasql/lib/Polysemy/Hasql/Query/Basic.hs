module Polysemy.Hasql.Query.Basic where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.StoreQuery (StoreQuery(Basic))

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)

interpretStoreQueryWith ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (ManagedTable dIn !! e) r =>
  Statement qIn [dIn] ->
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut dResult !! e) r
interpretStoreQueryWith statement result fromQ toD =
  interpretResumable \case
    Basic params ->
      result . fmap toD <$> restop (ManagedTable.runStatement (fromQ params) statement)

interpretStoreQuery ::
  ∀ q d a e r .
  Member (ManagedTable d !! e) r =>
  Statement q a ->
  InterpreterFor (StoreQuery q a !! e) r
interpretStoreQuery statement =
  interpretResumable \case
    Basic params ->
      restop (ManagedTable.runStatement params statement)
