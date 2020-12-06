module Polysemy.Hasql.ManagedTable where

import Polysemy.Resume (interpretResumable, restop, type (!))

import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Table.TableStructure (GenTableStructure, genTableStructure)

interpretManagedTable ::
  Member (Database ! e) r =>
  TableStructure ->
  InterpreterFor (ManagedTable d ! e) r
interpretManagedTable table =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.run (Just table) q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.retrying (Just table) interval q stmt)

interpretManagedTableGen ::
  ∀ rep d e r .
  GenTableStructure rep d =>
  Member (Database ! e) r =>
  InterpreterFor (ManagedTable d ! e) r
interpretManagedTableGen =
  interpretManagedTable (genTableStructure @rep @d)

interpretManagedTableUnmanaged ::
  Member (Database ! e) r =>
  InterpreterFor (ManagedTable d ! e) r
interpretManagedTableUnmanaged =
  interpretResumable \case
    ManagedTable.RunStatement q stmt ->
      restop (Database.run Nothing q stmt)
    ManagedTable.RetryStatement interval q stmt ->
      restop (Database.retrying Nothing interval q stmt)
