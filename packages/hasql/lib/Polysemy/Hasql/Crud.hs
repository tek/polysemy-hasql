module Polysemy.Hasql.Crud where

import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Crud (Crud(..))
import Polysemy.Hasql.Data.Table (Table(Table))
import qualified Polysemy.Hasql.Statement as Statement

interpretSchema ::
  QueryTable p d ->
  InterpreterFor (Crud p d) r
interpretSchema qTable@(QueryTable table@(Table structure row _) _ _) =
  interpret $ pure . \case
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

interpretSchemaSingleton ::
  Table d ->
  InterpreterFor (Crud () d) r
interpretSchemaSingleton table@(Table structure row _) =
  interpret $ pure . \case
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
