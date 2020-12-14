module Polysemy.Hasql.Query.One where

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
  ∀ rep qOut qIn dIn dOut e r .
  GenQueryTable rep qIn dIn =>
  Member (ManagedTable dIn !! e) r =>
  (qOut -> qIn) ->
  (dIn -> dOut) ->
  InterpreterFor (StoreQuery qOut (Maybe dOut) !! e) r
interpretOneGenAs fromQ toD =
  interpretOneAs fromQ toD (genQueryTable @rep @qIn @dIn)

interpretOneGenUidAs ::
  ∀ rep ir i d qOut qIn e r .
  GenQueryTable (UidRep ir rep) qIn (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  (qOut -> qIn) ->
  InterpreterFor (StoreQuery qOut (Maybe (Uid i d)) !! e) r
interpretOneGenUidAs fromQ =
  interpretOneAs fromQ id (genQueryTable @(UidRep ir rep) @qIn @(Uid i d))

interpretOneGenUid ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  InterpreterFor (StoreQuery q (Maybe (Uid i d)) !! e) r
interpretOneGenUid =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) @q @(Uid i d))

interpretOneGenUidWith ::
  ∀ rep ir i q d e r .
  GenQueryTable (UidRep ir rep) q (Uid i d) =>
  Member (ManagedTable (Uid i d) !! e) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q (Maybe (Uid i d)) !! e) r
interpretOneGenUidWith struct =
  interpretOneAs id id (genQueryTable @(UidRep ir rep) & QueryTable.table . Table.structure .~ struct)

interpretOne ::
  ∀ q d e r .
  Member (ManagedTable d !! e) r =>
  QueryTable q d ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOne =
  interpretOneAs id id

interpretOneWith ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d !! e) r =>
  TableStructure ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOneWith struct =
  interpretOne (genQueryTable @rep & QueryTable.table . Table.structure .~ struct)

interpretOneGen ::
  ∀ rep q d e r .
  GenQueryTable rep q d =>
  Member (ManagedTable d !! e) r =>
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretOneGen =
  interpretOne (genQueryTable @rep)
