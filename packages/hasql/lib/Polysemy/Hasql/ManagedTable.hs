module Polysemy.Hasql.ManagedTable where

import Control.Lens (mapMOf)
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Log (Log)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import Polysemy.Hasql.Data.DbType (Column(Column), Name(Name))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.InitDbError (initDbError)
import Polysemy.Hasql.Table (initTable)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema, basicSchema)

interpretManagedTable ::
  ∀ d r .
  Members [Database !! DbError, Log, Embed IO] r =>
  Table d ->
  InterpreterFor (ManagedTable d !! DbError) r
interpretManagedTable table@(Table column@(Column (Name name) _ _ _ _) _ _) =
  interpretResumable \case
    ManagedTable.Table ->
      restop (mapMOf Table.name Database.name table)
    ManagedTable.RunStatement q stmt ->
      restop (Database.withInit initDb (Database.runStatement q stmt))
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.withInit initDb (Database.runStatementRetrying interval q stmt))
  where
    initDb =
      InitDb name \ c -> initTable c column
{-# inline interpretManagedTable #-}

interpretManagedTableGen ::
  ∀ rep d r .
  BasicSchema rep d =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (ManagedTable d !! DbError) r
interpretManagedTableGen =
  interpretManagedTable (basicSchema @rep @d)
{-# inline interpretManagedTableGen #-}

interpretManagedTableAuto ::
  ∀ d r .
  BasicSchema Auto d =>
  Members [Database !! DbError, Log, Embed IO] r =>
  InterpreterFor (ManagedTable d !! DbError) r
interpretManagedTableAuto =
  interpretManagedTable (basicSchema @Auto @d)
{-# inline interpretManagedTableAuto #-}

-- TODO add effect for this, rename ManagedTable to Table
interpretManagedTableUnmanaged ::
  ∀ rep d e r .
  BasicSchema rep d =>
  Member (Database !! e) r =>
  InterpreterFor (ManagedTable d !! e) r
interpretManagedTableUnmanaged =
  interpretResumable \case
    ManagedTable.Table ->
      restop (mapMOf Table.name Database.name table)
    ManagedTable.RunStatement q stmt ->
      restop (Database.runStatement q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.runStatementRetrying interval q stmt)
  where
    table =
      basicSchema @rep @d

interpretManagedTableUnmanagedAuto ::
  ∀ d e r .
  BasicSchema Auto d =>
  Member (Database !! e) r =>
  InterpreterFor (ManagedTable d !! e) r
interpretManagedTableUnmanagedAuto =
  interpretManagedTableUnmanaged @Auto

queryTable ::
  ∀ q d e r .
  Show e =>
  Members [Query q d, ManagedTable d !! e, Error InitDbError] r =>
  Sem r (QueryTable q d)
queryTable =
  QueryTable <$> initDbError (ManagedTable.table @d) <*> Query.params <*> Query.query
