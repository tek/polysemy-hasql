module Polysemy.Hasql.Crud where

import Hasql.Encoders (Params)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.Uid (Uid)

import Polysemy.Hasql.Data.Crud (Crud (..))
import Polysemy.Hasql.Data.ManagedTable (ManagedTable, ManagedTableUid)
import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query, UidQuery)
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable, _qparams, _qwhere))
import Polysemy.Hasql.Data.Table (Table (Table))
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement

interpretCrudWith ::
  QueryTable q d ->
  Params i ->
  Where i d ->
  InterpreterFor (Crud i d q !! e) r
interpretCrudWith qTable@(QueryTable table@(Table structure row _) _ _) iParams iWhere =
  interpretResumable $ pure . \case
    Fetch ->
      Statement.selectWhere iQueryTable
    FetchAll ->
      Statement.select structure row
    FetchQ ->
      Statement.selectWhere qTable
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.upsert table
    Delete ->
      Statement.deleteWhere iQueryTable
    DeleteAll ->
      Statement.deleteAll table
  where
    iQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudWith #-}

interpretCrud ::
  ∀ i d q e r .
  Show e =>
  Members [Tagged "id" (Query i d), Tagged "main" (Query q d), ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud i d q !! e) r
interpretCrud sem = do
  table <- tag @"main" queryTable
  iParams <- tag @"id" (Query.params @i @d)
  iWhere <- tag @"id" Query.query
  interpretCrudWith table iParams iWhere sem
{-# inline interpretCrud #-}

interpretCrudUidWith ::
  QueryTable q (Uid i d) ->
  Params i ->
  Where i (Uid i d) ->
  InterpreterFor (Crud i (Uid i d) q !! e) r
interpretCrudUidWith qTable@(QueryTable table@(Table structure row _) _ _) iParams iWhere =
  interpretResumable $ pure . \case
    Fetch ->
      Statement.selectWhere uidQueryTable
    FetchAll ->
      Statement.select structure row
    FetchQ ->
      Statement.selectWhere qTable
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.upsert table
    Delete ->
      Statement.deleteWhere uidQueryTable
    DeleteAll ->
      Statement.deleteAll table
  where
    uidQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudUidWith #-}

interpretCrudUidNoUpdateWith ::
  QueryTable q (Uid i d) ->
  Params i ->
  Where i (Uid i d) ->
  InterpreterFor (Crud i (Uid i d) q !! e) r
interpretCrudUidNoUpdateWith qTable@(QueryTable table@(Table structure row _) _ _) iParams iWhere =
  interpretResumable $ pure . \case
    Fetch ->
      Statement.selectWhere uidQueryTable
    FetchAll ->
      Statement.select structure row
    FetchQ ->
      Statement.selectWhere qTable
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.upsert table
    Delete ->
      Statement.deleteWhere uidQueryTable
    DeleteAll ->
      Statement.deleteAll table
  where
    uidQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudUidNoUpdateWith #-}

interpretCrudUid ::
  ∀ i d q e r .
  Show e =>
  Members [Tagged "id" (UidQuery i d), Tagged "main" (Query q (Uid i d)), ManagedTableUid i d !! e, Error InitDbError] r =>
  InterpreterFor (Crud i (Uid i d) q !! e) r
interpretCrudUid sem = do
  table <- tag @"main" queryTable
  iParams <- tag @"id" (Query.params @i @(Uid i d))
  iWhere <- tag @"id" Query.query
  interpretCrudUidWith table iParams iWhere sem
{-# inline interpretCrudUid #-}

interpretCrudUidId ::
  ∀ i d e r .
  Show e =>
  Members [UidQuery i d, ManagedTableUid i d !! e, Error InitDbError] r =>
  InterpreterFor (Crud i (Uid i d) i !! e) r
interpretCrudUidId sem = do
  table <- queryTable
  iParams <- Query.params @i @(Uid i d)
  iWhere <- Query.query
  interpretCrudUidWith table iParams iWhere sem
{-# inline interpretCrudUidId #-}

interpretCrudSingletonWith ::
  Table d ->
  InterpreterFor (Crud () d () !! e) r
interpretCrudSingletonWith table@(Table structure row _) =
  interpretResumable $ pure . \case
    Fetch ->
      Statement.select structure row
    FetchAll ->
      Statement.select structure row
    FetchQ ->
      Statement.select structure row
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.insert table
    Delete ->
      Statement.deleteAll table
    DeleteAll ->
      Statement.deleteAll table
{-# inline interpretCrudSingletonWith #-}

interpretCrudSingleton ::
  Show e =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud () d () !! e) r
interpretCrudSingleton sem = do
  QueryTable table _ _ <- queryTable
  interpretCrudSingletonWith table sem
{-# inline interpretCrudSingleton #-}
