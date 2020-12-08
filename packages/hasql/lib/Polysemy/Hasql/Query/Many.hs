module Polysemy.Hasql.Query.Many where

import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Statement (selectWhere)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

interpretManyAs ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (ManagedTable dIn ! e) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut dResult ! e) r
interpretManyAs result fromQ toD table =
  interpretResumable \case
    Basic params ->
      result . fmap toD <$> restop (ManagedTable.runStatement (fromQ params) (selectWhere table))

interpretManyAsList ::
  ∀ qOut qIn dIn dOut e r .
  Member (ManagedTable dIn ! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut [dOut] ! e) r
interpretManyAsList =
  interpretManyAs id

-- TODO Managed
interpretManyGenAs ::
  ∀ dIn dOut rep qOut qIn e r .
  GenQueryTable rep qIn dIn =>
  Member (ManagedTable dIn ! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut [dOut] ! e) r
interpretManyGenAs fromQ toD =
  interpretManyAsList fromQ toD (genQueryTable @rep @qIn @dIn)

interpretManyGenUidAs ::
  ∀ rep ir i qOut qIn d e r .
  GenQueryTable (UidRep ir rep) qIn (Uid i d) =>
  Member (ManagedTable (Uid i d) ! e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut [Uid i d] ! e) r
interpretManyGenUidAs fromQ =
  interpretManyAsList fromQ id (genQueryTable @(UidRep ir rep) @qIn @(Uid i d))

interpretManyGenUid ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) ! e) r =>
  InterpreterFor (StoreQuery q [Uid i d] ! e) r
interpretManyGenUid =
  interpretManyAsList id id (genQueryTable @(UidRep ir rep) @q @(Uid i d))

interpretMany ::
  Member (ManagedTable d ! e) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q [d] ! e) r
interpretMany =
  interpretManyAsList id id

interpretManyWith ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d ! e) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q [d] ! e) r
interpretManyWith struct =
  interpretMany (genQueryTable @rep & QueryTable.table . Table.structure .~ struct)

interpretManyGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d ! e) r =>
  InterpreterFor (StoreQuery q [d] ! e) r
interpretManyGen =
  interpretMany (genQueryTable @rep)
