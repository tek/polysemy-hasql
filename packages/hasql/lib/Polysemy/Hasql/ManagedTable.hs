module Polysemy.Hasql.ManagedTable where

import Polysemy.Db.Data.DbError (DbError)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table (initTable)
import Polysemy.Hasql.Column.DataColumn (TableStructure, tableStructure)
import Polysemy.Hasql.Data.DbType (Column(Column), Name(Name))

interpretManagedTable ::
  ∀ d r .
  Members [Database !! DbError, Embed IO] r =>
  Column ->
  InterpreterFor (ManagedTable d !! DbError) r
interpretManagedTable table@(Column (Name name) _ _ _ _) =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.withInit initDb (Database.runStatement q stmt))
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.withInit initDb (Database.runStatementRetrying interval q stmt))
  where
    initDb =
      InitDb name \c -> initTable c table

interpretManagedTableGen ::
  ∀ rep d r .
  TableStructure rep d =>
  Members [Database !! DbError, Embed IO] r =>
  InterpreterFor (ManagedTable d !! DbError) r
interpretManagedTableGen =
  interpretManagedTable (tableStructure @rep @d)

interpretManagedTableUnmanaged ::
  Member (Database !! e) r =>
  InterpreterFor (ManagedTable d !! e) r
interpretManagedTableUnmanaged =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.runStatement q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.runStatementRetrying interval q stmt)
