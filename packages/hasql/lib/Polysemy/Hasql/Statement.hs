module Polysemy.Hasql.Statement where

import Hasql.Decoders (Row, noResult)
import Hasql.Encoders (Params, noParams)
import Hasql.Statement (Statement(Statement))
import Prelude hiding (All, Compose, Generic)

import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Text.Quote (dquote)
import qualified Polysemy.Hasql.ColumnParams as ColumnParams
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.QueryWhere (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Table.Query.Delete (deleteSql)
import Polysemy.Hasql.Table.Query.Fragment (addFragment, alterFragment, conflictFragment)
import qualified Polysemy.Hasql.Table.Query.Insert as Query (insert)
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import Polysemy.Hasql.Table.Query.Set (set)
import Polysemy.Hasql.Table.Query.Text (commaSeparated, commaSeparatedSql)
import Polysemy.Hasql.Table.ResultShape (ResultShape(resultShape))

query ::
  ResultShape d result =>
  SqlCode ->
  Row d ->
  Params query ->
  Statement query result
query (SqlCode sql) row params =
  Statement (encodeUtf8 sql) params (resultShape row) True

plain :: SqlCode -> Statement () ()
plain (SqlCode sql) =
  Statement (encodeUtf8 sql) mempty noResult False

select ::
  ∀ d .
  TableStructure ->
  Row d ->
  Statement () [d]
select table row =
  query (selectColumns table) row noParams

selectWhereSql ::
  QueryTable query d ->
  SqlCode
selectWhereSql (QueryTable (Table (selectColumns -> SqlCode sel) _ _) _ (QueryWhere (SqlCode qw))) =
  SqlCode $ [i|#{sel} where #{qw}|]

-- |Construct a query of the shape "select ... from ... where ..."
-- The point of the 'ResultShape' constraint is to help with inferring the @d@ type from the @result@ type so no manual
-- type application is necessary.
selectWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
selectWhere table@(QueryTable (Table _ row _) params _) =
  query (selectWhereSql table) row params

insert ::
  Table d ->
  Statement d ()
insert (Table structure row params) =
  query (Query.insert structure) row params

upsertSql ::
  TableStructure ->
  SqlCode
upsertSql table@(TableStructure _ columns) =
  SqlCode [i|#{ins} #{conflict}|]
  where
    SqlCode conflict =
      conflictFragment columns st
    SqlCode ins =
      Query.insert table
    st =
      set table

upsert ::
  Table d ->
  Statement d ()
upsert (Table structure row params) =
  query (upsertSql structure) row params

deleteWhereSql ::
  QueryTable d q ->
  SqlCode
deleteWhereSql (QueryTable (Table structure _ _) _ (QueryWhere (SqlCode qw))) =
  SqlCode $ [i|#{del} where #{qw}|]
  where
    SqlCode del =
      deleteSql structure

deleteWhere ::
  QueryTable p d ->
  Statement p ()
deleteWhere table@(QueryTable (Table _ row _) params _) =
  query (deleteWhereSql table) row params

deleteAll ::
  Table d ->
  Statement () ()
deleteAll (Table structure row _) =
  query (deleteSql structure) row noParams

createTableSql ::
  TableStructure ->
  SqlCode
createTableSql (TableStructure (TableName (dquote -> name)) (Columns columns)) =
  SqlCode [i|create table #{name} (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (formattedColumn <$> columns))
    formattedColumn (Column (dquote -> n) t (ColumnParams.format -> params)) =
      [i|#{n} #{t}#{params}|]

createTable ::
  TableStructure ->
  Statement () ()
createTable =
  plain . createTableSql

dropTableSql ::
  TableName ->
  SqlCode
dropTableSql (TableName (dquote -> name)) =
   SqlCode [i|drop table if exists #{name}|]

dropTable ::
  TableName ->
  Statement () ()
dropTable =
  plain . dropTableSql

alterSql ::
  TableName ->
  NonEmpty Column ->
  SqlCode
alterSql (alterFragment -> SqlCode alter') (toList -> columns) =
  SqlCode [i|#{alter'} #{colAdds}|]
  where
    SqlCode colAdds =
      commaSeparatedSql (addFragment <$> columns)

alter ::
  TableName ->
  NonEmpty Column ->
  Statement () ()
alter =
  plain .: alterSql

createDbSql ::
  DbName ->
  SqlCode
createDbSql (DbName (dquote -> name)) =
  SqlCode [i|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  plain . createDbSql

dropDbSql ::
  DbName ->
  SqlCode
dropDbSql (DbName (dquote -> name)) =
  SqlCode [i|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  plain . dropDbSql
