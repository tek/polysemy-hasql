module Polysemy.Hasql.Crud where

import Hasql.Encoders (Params)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.Partial (getPartial)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Tagged (Tagged, tag)

import Polysemy.Hasql.Data.Crud (Crud (..))
import Polysemy.Hasql.Data.ManagedTable (ManagedTable, ManagedTableUid)
import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query, UidQuery)
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable, _qparams, _qwhere))
import Polysemy.Hasql.Data.Table (Table (Table))
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Hasql.Statement (Statement(Statement))
import Hasql.Decoders (singleRow)

interpretCrudWith ::
  BuildPartialSql p tree =>
  QueryTable q d ->
  Params i ->
  Where i d ->
  InterpreterFor (Crud i d q p !! e) r
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
    Update i patch ->
      Statement.update iQueryTable i (getPartial patch)
    UpdateQ q patch ->
      Statement.update qTable q (getPartial patch)
  where
    iQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudWith #-}

interpretCrud ::
  ∀ i d q p e r tree .
  Show e =>
  BuildPartialSql p tree =>
  Members [Tagged "id" (Query i d), Tagged "main" (Query q d), ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud i d q p !! e) r
interpretCrud sem = do
  table <- tag @"main" queryTable
  iParams <- tag @"id" (Query.params @i @d)
  iWhere <- tag @"id" Query.query
  interpretCrudWith table iParams iWhere sem
{-# inline interpretCrud #-}

interpretCrudUidWith ::
  BuildPartialSql p tree =>
  QueryTable q (Uid i d) ->
  Params i ->
  Where i (Uid i d) ->
  InterpreterFor (Crud i (Uid i d) q p !! e) r
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
    Update i patch ->
      Statement.update uidQueryTable i (getPartial patch)
    UpdateQ q patch ->
      Statement.update qTable q (getPartial patch)
  where
    uidQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudUidWith #-}

interpretCrudUidNoUpdateWith ::
  QueryTable q (Uid i d) ->
  Params i ->
  Where i (Uid i d) ->
  InterpreterFor (Crud i (Uid i d) q p !! e) r
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
    Update _ _ ->
      Statement "select 1" mempty (singleRow (pure Nothing)) True
    UpdateQ _ _ ->
      Statement "select 1" mempty (singleRow (pure Nothing)) True
  where
    uidQueryTable =
      qTable { _qparams = iParams, _qwhere = iWhere }
{-# inline interpretCrudUidNoUpdateWith #-}

interpretCrudUid ::
  ∀ i d q p e r tree .
  Show e =>
  BuildPartialSql p tree =>
  Members [Tagged "id" (UidQuery i d), Tagged "main" (Query q (Uid i d)), ManagedTableUid i d !! e, Error InitDbError] r =>
  InterpreterFor (Crud i (Uid i d) q p !! e) r
interpretCrudUid sem = do
  table <- tag @"main" queryTable
  iParams <- tag @"id" (Query.params @i @(Uid i d))
  iWhere <- tag @"id" Query.query
  interpretCrudUidWith table iParams iWhere sem
{-# inline interpretCrudUid #-}

interpretCrudSingletonWith ::
  BuildPartialSql p tree =>
  Table d ->
  InterpreterFor (Crud () d () p !! e) r
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
    Update _ patch ->
      Statement.updateSingle table (getPartial patch)
    UpdateQ _ patch ->
      Statement.updateSingle table (getPartial patch)
{-# inline interpretCrudSingletonWith #-}

interpretCrudSingleton ::
  Show e =>
  BuildPartialSql p tree =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud () d () p !! e) r
interpretCrudSingleton sem = do
  QueryTable table _ _ <- queryTable
  interpretCrudSingletonWith table sem
{-# inline interpretCrudSingleton #-}
