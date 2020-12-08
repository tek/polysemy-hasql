module Polysemy.Hasql.ManagedTable where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableName (TableName(TableName))
import qualified Polysemy.Db.Data.TableStructure as TableStructure (TableStructure(_name))
import Polysemy.Resume (interpretResumable, restop, type (!))

import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table (initTable)
import Polysemy.Hasql.Table.TableStructure (GenTableStructure, genTableStructure)

interpretManagedTable ::
  ∀ d r .
  Members [Database ! DbError, Embed IO] r =>
  TableStructure ->
  InterpreterFor (ManagedTable d ! DbError) r
interpretManagedTable table =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.withInit initDb (Database.runStatement q stmt))
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.withInit initDb (Database.runStatementRetrying interval q stmt))
  where
    initDb =
      InitDb clientId \c -> initTable c table
    TableName clientId =
      TableStructure._name table

interpretManagedTableGen ::
  ∀ rep d r .
  GenTableStructure rep d =>
  Members [Database ! DbError, Embed IO] r =>
  InterpreterFor (ManagedTable d ! DbError) r
interpretManagedTableGen =
  interpretManagedTable (genTableStructure @rep @d)

interpretManagedTableUnmanaged ::
  Member (Database ! e) r =>
  InterpreterFor (ManagedTable d ! e) r
interpretManagedTableUnmanaged =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.runStatement q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.runStatementRetrying interval q stmt)
