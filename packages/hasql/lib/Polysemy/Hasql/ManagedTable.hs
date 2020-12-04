module Polysemy.Hasql.ManagedTable where

import Polysemy.Resume (interpretResumable, restop, type (!))

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table.TableStructure (GenTableStructure, genTableStructure)

interpretManagedTable ::
  Member (Database ! DbError) r =>
  TableStructure ->
  InterpreterFor (ManagedTable d ! DbError) r
interpretManagedTable table =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.run (Just table) q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.retrying (Just table) interval q stmt)

interpretManagedTableGen ::
  ∀ rep d r .
  GenTableStructure rep d =>
  Member (Database ! DbError) r =>
  InterpreterFor (ManagedTable d ! DbError) r
interpretManagedTableGen =
  interpretManagedTable (genTableStructure @rep @d)

interpretManagedTableUnmanaged ::
  Member (Database ! DbError) r =>
  InterpreterFor (ManagedTable d ! DbError) r
interpretManagedTableUnmanaged =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.run Nothing q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.retrying Nothing interval q stmt)
