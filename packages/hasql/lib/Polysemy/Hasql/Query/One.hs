module Polysemy.Hasql.Query.One where

import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Statement (selectWhere)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

import Polysemy.Hasql.Data.DbType (Column)

interpretOneAs ::
  ∀ qOut qIn dIn dOut e r .
  Member (ManagedTable dIn !! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut (Maybe dOut) !! e) r
interpretOneAs fromQ toD table =
  interpretResumable \case
    Basic params ->
      fmap toD <$> restop (ManagedTable.runStatement (fromQ params) (selectWhere table))

interpretOneGenAs ::
  ∀ qrep rep qOut qIn dIn dOut e r .
  GenQueryTable qrep rep qIn dIn =>
  Member (ManagedTable dIn !! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut (Maybe dOut) !! e) r
interpretOneGenAs fromQ toD =
  interpretOneAs fromQ toD (genQueryTable @qrep @rep @qIn @dIn)

interpretOneGenUidAs ::
  ∀ qrep rep ir i d qOut qIn e r .
  GenQueryTable qrep (UidRep ir rep) qIn (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut (Maybe (Uid i d)) !! e) r
interpretOneGenUidAs fromQ =
  interpretOneAs fromQ id (genQueryTable @qrep @(UidRep ir rep) @qIn @(Uid i d))

interpretOneGenUid ::
  ∀ qrep rep ir i q d e r .
  GenQueryTable qrep (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  InterpreterFor (StoreQuery q (Maybe (Uid i d)) !! e) r
interpretOneGenUid =
  interpretOneAs id id (genQueryTable @qrep @(UidRep ir rep) @q @(Uid i d))

interpretOneGenUidWith ::
  ∀ qrep rep ir i q d e r .
  GenQueryTable qrep (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  Column ->
  InterpreterFor (StoreQuery q (Maybe (Uid i d)) !! e) r
interpretOneGenUidWith struct =
  interpretOneAs id id (genQueryTable @qrep @(UidRep ir rep) & QueryTable.table . Table.structure .~ struct)

interpretOne ::
  ∀ q d e r .
  Member (ManagedTable d !! e) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOne =
  interpretOneAs id id

interpretOneWith ::
  ∀ qrep rep q d e r .
  GenQueryTable qrep rep q d =>
  Member (ManagedTable d !! e) r =>
  Column ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOneWith struct =
  interpretOne (genQueryTable @qrep @rep & QueryTable.table . Table.structure .~ struct)

interpretOneGen ::
  ∀ qrep rep q d e r .
  GenQueryTable qrep rep q d =>
  Member (ManagedTable d !! e) r =>
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOneGen =
  interpretOne (genQueryTable @qrep @rep)
