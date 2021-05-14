module Polysemy.Hasql.Crud where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Hasql.Data.Crud (Crud(..))
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement

interpretCrudWith ::
  QueryTable q d ->
  InterpreterFor (Crud q d !! e) r
interpretCrudWith qTable@(QueryTable table@(Table structure row _) _ _) =
  interpretResumable $ pure . \case
    Fetch ->
      Statement.selectWhere qTable
    FetchAll ->
      Statement.select structure row
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.upsert table
    Delete ->
      Statement.deleteWhere qTable
    DeleteAll ->
      Statement.deleteAll table
    -- Update ->
    --   undefined
      -- Statement.update table
{-# INLINE interpretCrudWith #-}

interpretCrud ::
  ∀ q d e r .
  Show e =>
  Members [Query q d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud q d !! e) r
interpretCrud sem = do
  table <- queryTable
  interpretCrudWith table sem
{-# INLINE interpretCrud #-}

interpretCrudSingletonWith ::
  Table d ->
  InterpreterFor (Crud () d !! e) r
interpretCrudSingletonWith table@(Table structure row _) =
  interpretResumable $ pure . \case
    Fetch ->
      listToMaybe <$> Statement.select structure row
    FetchAll ->
      Statement.select structure row
    Insert ->
      Statement.insert table
    Upsert ->
      Statement.insert table
    Delete ->
      Statement.deleteAll table
    DeleteAll ->
      Statement.deleteAll table
    -- Update ->
    --   undefined
      -- Statement.update table
{-# INLINE interpretCrudSingletonWith #-}

interpretCrudSingleton ::
  Show e =>
  Members [Query () d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (Crud () d !! e) r
interpretCrudSingleton sem = do
  QueryTable table _ _ <- queryTable
  interpretCrudSingletonWith table sem
{-# INLINE interpretCrudSingleton #-}
