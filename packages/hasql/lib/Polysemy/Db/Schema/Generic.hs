module Polysemy.Db.Schema.Generic where

import Polysemy.Db.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Db.Data.Schema (Schema(..))
import Polysemy.Db.Data.Table (Table(Table))
import qualified Polysemy.Db.Statement as Statement

interpretSchema ::
  QueryTable d p ->
  InterpreterFor (Schema p d) r
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

interpretSchemaSingleton ::
  Table d ->
  InterpreterFor (Schema () d) r
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
