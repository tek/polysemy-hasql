module Polysemy.Hasql.Query.Many where

import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.Uid (Uid)

import Polysemy.Hasql.Data.DbType (Column)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Query.Basic (interpretStoreQueryWith)
import Polysemy.Hasql.Statement (selectWhere)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

interpretManyAs ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (ManagedTable dIn !! e) r =>
  QueryTable qIn dIn ->
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut dResult !! e) r
interpretManyAs table =
  interpretStoreQueryWith (selectWhere table)

interpretManyAsList ::
  ∀ qOut qIn dIn dOut e r .
  Member (ManagedTable dIn !! e) r =>
  QueryTable qIn dIn ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut [dOut] !! e) r
interpretManyAsList table =
  interpretManyAs table id

interpretManyGenAs ::
  ∀ dIn dOut qrep rep qOut qIn e r .
  GenQueryTable qrep rep qIn dIn =>
  Member (ManagedTable dIn !! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut [dOut] !! e) r
interpretManyGenAs =
  interpretManyAsList (genQueryTable @qrep @rep @qIn @dIn)

interpretManyGenUidAs ::
  ∀ qrep rep ir i qOut qIn d e r .
  GenQueryTable qrep (UidRep ir rep) qIn (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut [Uid i d] !! e) r
interpretManyGenUidAs fromQ =
  interpretManyAsList (genQueryTable @qrep @(UidRep ir rep) @qIn @(Uid i d)) fromQ id

interpretManyGenUid ::
  ∀ qrep rep ir i q d e r .
  GenQueryTable qrep (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  InterpreterFor (StoreQuery q [Uid i d] !! e) r
interpretManyGenUid =
  interpretManyAsList (genQueryTable @qrep @(UidRep ir rep) @q @(Uid i d)) id id

interpretMany ::
  Member (ManagedTable d !! e) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretMany table =
  interpretManyAsList table id id

interpretManyWith ::
  ∀ qrep rep q d e r .
  GenQueryTable qrep rep q d =>
  Member (ManagedTable d !! e) r =>
  Column ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretManyWith struct =
  interpretMany (genQueryTable @qrep @rep & QueryTable.table . Table.structure .~ struct)

interpretManyGen ::
  ∀ qrep rep q d e r .
  GenQueryTable qrep rep q d =>
  Member (ManagedTable d !! e) r =>
  InterpreterFor (StoreQuery q [d] !! e) r
interpretManyGen =
  interpretMany (genQueryTable @qrep @rep)
