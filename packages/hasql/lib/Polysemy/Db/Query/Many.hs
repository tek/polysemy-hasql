module Polysemy.Db.Query.Many where

import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Data.Column (PK, PKRep, pkToUid)
import qualified Polysemy.Db.Data.Database as Database
import Polysemy.Db.Data.Database (Database)
import Polysemy.Db.Data.QueryTable (QueryTable)
import Polysemy.Db.Statement (selectWhere)
import Polysemy.Db.Table.QueryTable (GenQueryTable, genQueryTable)

execute ::
  ∀ dIn dOut qOut qIn dResult e r .
  Member (Database dIn e) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable dIn qIn ->
  qOut ->
  Sem r (Either (StoreError e) dResult)
execute result fromQ toD table params =
  bimap StoreError.Backend (result . fmap toD) <$> Database.run (fromQ params) (selectWhere table)

interpretManyWith ::
  ∀ dIn dOut qOut qIn dResult e r .
  Member (Database dIn e) r =>
  ([dOut] -> dResult) ->
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable dIn qIn ->
  InterpreterFor (StoreQuery qOut dResult e) r
interpretManyWith result fromQ toD table =
  interpret \case
    Basic params ->
      execute result fromQ toD table params

interpretManyAs ::
  ∀ dIn dOut qOut qIn e r .
  Member (Database dIn e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable dIn qIn ->
  InterpreterFor (StoreQuery qOut [dOut] e) r
interpretManyAs =
  interpretManyWith id

interpretManyGenAs ::
  ∀ dIn dOut rep qOut qIn e r .
  GenQueryTable dIn rep qIn =>
  Member (Database dIn e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut [dOut] e) r
interpretManyGenAs fromQ toD =
  interpretManyAs fromQ toD (genQueryTable @dIn @rep @qIn)

interpretManyGenUidAs ::
  ∀ d rep i qOut qIn e r .
  GenQueryTable (PK i d) (PKRep i rep) qIn =>
  Member (Database (PK i d) e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut [Uid i d] e) r
interpretManyGenUidAs fromQ =
  interpretManyAs fromQ pkToUid (genQueryTable @(PK i d) @(PKRep i rep) @qIn)

interpretManyGenUid ::
  ∀ d rep i q e r .
  GenQueryTable (PK i d) (PKRep i rep) q =>
  Member (Database (PK i d) e) r =>
  InterpreterFor (StoreQuery q [Uid i d] e) r
interpretManyGenUid =
  interpretManyAs id pkToUid (genQueryTable @(PK i d) @(PKRep i rep) @q)

interpretMany ::
  Member (Database d e) r =>
  QueryTable d q ->
  InterpreterFor (StoreQuery q [d] e) r
interpretMany table =
  interpret \case
    Basic params ->
      mapLeft StoreError.Backend <$> Database.run params (selectWhere table)

interpretManyGen ::
  ∀ d rep q e r .
  GenQueryTable d rep q =>
  Member (Database d e) r =>
  InterpreterFor (StoreQuery q [d] e) r
interpretManyGen =
  interpretMany (genQueryTable @_ @rep)
