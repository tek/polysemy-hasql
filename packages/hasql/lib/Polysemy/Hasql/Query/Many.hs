module Polysemy.Hasql.Query.Many where

import Polysemy.Db.Data.Column (PK, PKRep, pkToUid)
import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Statement (selectWhere)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

execute ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (Database e dIn) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  qOut ->
  Sem r (Either (StoreError e) dResult)
execute result fromQ toD table params =
  bimap StoreError.Backend (result . fmap toD) <$> Database.run (fromQ params) (selectWhere table)

interpretManyWith ::
  ∀ qOut qIn dIn dOut dResult e r .
  Member (Database e dIn) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e dResult) r
interpretManyWith result fromQ toD table =
  interpret \case
    Basic params ->
      execute result fromQ toD table params

interpretManyAs ::
  ∀ qOut qIn dIn dOut e r .
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable qIn dIn ->
  InterpreterFor (StoreQuery qOut e [dOut]) r
interpretManyAs =
  interpretManyWith id

interpretManyGenAs ::
  ∀ dIn dOut rep qOut qIn e r .
  GenQueryTable rep qIn dIn =>
  Member (Database e dIn) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut e [dOut]) r
interpretManyGenAs fromQ toD =
  interpretManyAs fromQ toD (genQueryTable @rep @qIn @dIn)

interpretManyGenUidAs ::
  ∀ rep i qOut qIn d e f r .
  GenQueryTable (PKRep f i rep) qIn (PK f i d) =>
  Member (Database e (PK f i d)) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut e [Uid i d]) r
interpretManyGenUidAs fromQ =
  interpretManyAs fromQ pkToUid (genQueryTable @(PKRep f i rep) @qIn @(PK f i d))

interpretManyGenUid ::
  ∀ rep i q d e f r .
  GenQueryTable (PKRep f i rep) q (PK f i d) =>
  Member (Database e (PK f i d)) r =>
  InterpreterFor (StoreQuery q e [Uid i d]) r
interpretManyGenUid =
  interpretManyAs id pkToUid (genQueryTable @(PKRep f i rep) @q @(PK f i d))

interpretMany ::
  Member (Database e d) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q e [d]) r
interpretMany table =
  interpret \case
    Basic params ->
      mapLeft StoreError.Backend <$> Database.run params (selectWhere table)

interpretManyGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (Database e d) r =>
  InterpreterFor (StoreQuery q e [d]) r
interpretManyGen =
  interpretMany (genQueryTable @rep)
