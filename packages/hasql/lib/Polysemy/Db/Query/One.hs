module Polysemy.Db.Query.One where

import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Data.Column (PK, PKRep, pkToUid)
import Polysemy.Db.Data.Database (Database)
import qualified Polysemy.Db.Data.Database as Database
import Polysemy.Db.Data.QueryTable (QueryTable)
import Polysemy.Db.Statement (selectWhere)
import Polysemy.Db.Table.QueryTable (GenQueryTable, genQueryTable)

interpretOneAs ::
  ∀ dIn dOut qOut qIn e r .
  Member (Database dIn e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  QueryTable dIn qIn ->
  InterpreterFor (StoreQuery qOut (Maybe dOut) e) r
interpretOneAs fromQ toD table =
  interpret \case
    Basic params ->
      bimap StoreError.Backend (fmap toD) <$> Database.run (fromQ params) (selectWhere table)

interpretOneGenAs ::
  ∀ dIn dOut rep qOut qIn e r .
  GenQueryTable dIn rep qIn =>
  Member (Database dIn e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut (Maybe dOut) e) r
interpretOneGenAs fromQ toD =
  interpretOneAs fromQ toD (genQueryTable @dIn @rep @qIn)

interpretOneGenUidAs ::
  ∀ d rep i qOut qIn e r .
  GenQueryTable (PK i d) (PKRep i rep) qIn =>
  Member (Database (PK i d) e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut (Maybe (Uid i d)) e) r
interpretOneGenUidAs fromQ =
  interpretOneAs fromQ pkToUid (genQueryTable @(PK i d) @(PKRep i rep) @qIn)

interpretOneGenUid ::
  ∀ d rep i q e r .
  GenQueryTable (PK i d) (PKRep i rep) q =>
  Member (Database (PK i d) e) r =>
  InterpreterFor (StoreQuery q (Maybe (Uid i d)) e) r
interpretOneGenUid =
  interpretOneAs id pkToUid (genQueryTable @(PK i d) @(PKRep i rep) @q)

interpretOne ::
  Member (Database d e) r =>
  QueryTable d q ->
  InterpreterFor (StoreQuery q (Maybe d) e) r
interpretOne table =
  interpret \case
    Basic params ->
      mapLeft StoreError.Backend <$> Database.run params (selectWhere table)

interpretOneGen ::
  ∀ d rep q e r .
  GenQueryTable d rep q =>
  Member (Database d e) r =>
  InterpreterFor (StoreQuery q (Maybe d) e) r
interpretOneGen =
  interpretOne (genQueryTable @_ @rep)
