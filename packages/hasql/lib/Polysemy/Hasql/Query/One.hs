module Polysemy.Hasql.Query.One where

import Polysemy.Db.Data.Column (UidRep)
import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Statement (selectWhere)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

interpretOneAs ::
  ∀ qOut qIn dIn dOut e r .
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e (Maybe dOut)) r
interpretOneAs fromQ toD table =
  interpret \case
    Basic params ->
      bimap StoreError.Backend (fmap toD) <$> Database.run (fromQ params) (selectWhere table)

interpretOneGenAs ::
  ∀ rep qOut qIn dIn dOut e r .
  GenQueryTable rep qIn dIn =>
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut e (Maybe dOut)) r
interpretOneGenAs fromQ toD =
  interpretOneAs fromQ toD (genQueryTable @rep @qIn @dIn)

interpretOneGenUidAs ::
  ∀ rep ir i d qOut qIn e r .
  GenQueryTable (UidRep ir rep) qIn (Uid i d) =>
  Member (Database e (Uid i d)) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut e (Maybe (Uid i d))) r
interpretOneGenUidAs fromQ =
  interpretOneAs fromQ id (genQueryTable @(UidRep ir rep) @qIn @(Uid i d))

interpretOneGenUid ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (Database e (Uid i d)) r =>
  InterpreterFor (StoreQuery q e (Maybe (Uid i d))) r
interpretOneGenUid =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) @q @(Uid i d))

interpretOneGenUidWith ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (Database e (Uid i d)) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q e (Maybe (Uid i d))) r
interpretOneGenUidWith struct =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) & QueryTable.table . Table.structure .~ struct)

interpretOne ::
  ∀ q d e r .
  Member (Database e d) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q e (Maybe d)) r
interpretOne =
  interpretOneAs id id

interpretOneWith ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (Database e d) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q e (Maybe d)) r
interpretOneWith struct =
  interpretOne (genQueryTable @rep & QueryTable.table . Table.structure .~ struct)

interpretOneGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (Database e d) r =>
  InterpreterFor (StoreQuery q e (Maybe d)) r
interpretOneGen =
  interpretOne (genQueryTable @rep)
