module Polysemy.Hasql.Query.Many where

import Polysemy.Db.Data.Column (PK, PKRep, pkToUid)
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

interpretManyAs ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (Database e dIn) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e dResult) r
interpretManyAs result fromQ toD table =
  interpret \case
    Basic params ->
      bimap StoreError.Backend (result . fmap toD) <$> Database.run (fromQ params) (selectWhere table)

interpretManyAsList ::
  ∀ qOut qIn dIn dOut e r .
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e [dOut]) r
interpretManyAsList =
  interpretManyAs id

interpretManyGenAs ::
  ∀ dIn dOut rep qOut qIn e r .
  GenQueryTable rep qIn dIn =>
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut e [dOut]) r
interpretManyGenAs fromQ toD =
  interpretManyAsList fromQ toD (genQueryTable @rep @qIn @dIn)

interpretManyGenUidAs ::
  ∀ rep i qOut qIn d e f r .
  GenQueryTable (PKRep f i rep) qIn (PK f i d) =>
  Member (Database e (PK f i d)) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut e [Uid i d]) r
interpretManyGenUidAs fromQ =
  interpretManyAsList fromQ pkToUid (genQueryTable @(PKRep f i rep) @qIn @(PK f i d))

interpretManyGenUid ::
  ∀ rep i q d e f r .
  GenQueryTable (PKRep f i rep) q (PK f i d) =>
  Member (Database e (PK f i d)) r =>
  InterpreterFor (StoreQuery q e [Uid i d]) r
interpretManyGenUid =
  interpretManyAsList id pkToUid (genQueryTable @(PKRep f i rep) @q @(PK f i d))

interpretMany ::
  Member (Database e d) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q e [d]) r
interpretMany =
  interpretManyAsList id id

interpretManyWith ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (Database e d) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q e [d]) r
interpretManyWith struct =
  interpretMany (genQueryTable @rep & QueryTable.table . Table.structure .~ struct)

interpretManyGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (Database e d) r =>
  InterpreterFor (StoreQuery q e [d]) r
interpretManyGen =
  interpretMany (genQueryTable @rep)
