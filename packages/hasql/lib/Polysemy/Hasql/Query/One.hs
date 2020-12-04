module Polysemy.Hasql.Query.One where

import Polysemy.Resume (interpretResumable, restop, type (!))

import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.DbError (DbError)
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

interpretOneAs ::
  ∀ qOut qIn dIn dOut e r .
  Member (ManagedTable dIn ! DbError) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e (Maybe dOut) ! DbError) r
interpretOneAs fromQ toD table =
  interpretResumable \case
    Basic params ->
      fmap toD <$> restop (ManagedTable.runStatement (fromQ params) (selectWhere table))

-- TODO Managed
interpretOneGenAs ::
  ∀ rep qOut qIn dIn dOut e r .
  GenQueryTable rep qIn dIn =>
  Member (ManagedTable dIn ! DbError) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut e (Maybe dOut) ! DbError) r
interpretOneGenAs fromQ toD =
  interpretOneAs fromQ toD (genQueryTable @rep @qIn @dIn)

interpretOneGenUidAs ::
  ∀ rep ir i d qOut qIn e r .
  GenQueryTable (UidRep ir rep) qIn (Uid i d) =>
  Member (ManagedTable (Uid i d) ! DbError) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut e (Maybe (Uid i d)) ! DbError) r
interpretOneGenUidAs fromQ =
  interpretOneAs fromQ id (genQueryTable @(UidRep ir rep) @qIn @(Uid i d))

interpretOneGenUid ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) ! DbError) r =>
  InterpreterFor (StoreQuery q e (Maybe (Uid i d)) ! DbError) r
interpretOneGenUid =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) @q @(Uid i d))

interpretOneGenUidWith ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) ! DbError) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q e (Maybe (Uid i d)) ! DbError) r
interpretOneGenUidWith struct =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) & QueryTable.table . Table.structure .~ struct)

interpretOne ::
  ∀ q d e r .
  Member (ManagedTable d ! DbError) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q e (Maybe d) ! DbError) r
interpretOne =
  interpretOneAs id id

interpretOneWith ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d ! DbError) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q e (Maybe d) ! DbError) r
interpretOneWith struct =
  interpretOne (genQueryTable @rep & QueryTable.table . Table.structure .~ struct)

interpretOneGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d ! DbError) r =>
  InterpreterFor (StoreQuery q e (Maybe d) ! DbError) r
interpretOneGen =
  interpretOne (genQueryTable @rep)
