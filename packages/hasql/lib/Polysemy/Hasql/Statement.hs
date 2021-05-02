module Polysemy.Hasql.Statement where

import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, int8, noResult, nullable)
import Hasql.Encoders (Params, noParams)
import Hasql.Statement (Statement(Statement))
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.PartialFields (PartialFields)
import Polysemy.Db.Text.Quote (dquote)

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column(Column), Name(Name), Selector(Selector))
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode, unSqlCode))
import Polysemy.Hasql.Data.Table (Table(Table, _row, _structure, _params, _partialParams), _partialParams)
import Polysemy.Hasql.Data.Where (Where(Where))
import Polysemy.Hasql.DbType (baseColumns, columnSpec, quotedName)
import Polysemy.Hasql.Table.Query.Delete (deleteSql)
import Polysemy.Hasql.Table.Query.Fragment (addColumnFragment, alterFragment, conflictFragment, fromFragment)
import qualified Polysemy.Hasql.Table.Query.Insert as Query (insert)
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import qualified Polysemy.Hasql.Table.Query.Set as Query (set)
import qualified Polysemy.Hasql.Table.Query.SetPartial as Query (setPartial)
import Polysemy.Hasql.Table.Query.Text (commaColumns, commaSeparated, commaSeparatedSql)
import qualified Polysemy.Hasql.Table.Query.Update as Query (update)
import Polysemy.Hasql.Table.ResultShape (ResultShape(resultShape))

query ::
  ResultShape d result =>
  SqlCode ->
  Row d ->
  Params p ->
  Statement p result
query (SqlCode sql) row params =
  Statement (encodeUtf8 sql) params (resultShape row) True

plain :: SqlCode -> Statement () ()
plain (SqlCode sql) =
  Statement (encodeUtf8 sql) mempty noResult False

select ::
  ∀ d .
  Column ->
  Row d ->
  Statement () [d]
select table row =
  query (selectColumns table) row noParams

selectWhereSql ::
  QueryTable query d ->
  SqlCode
selectWhereSql (QueryTable (Table (selectColumns -> SqlCode sel) _ _ _) _ (Where (SqlCode qw))) =
  SqlCode [qt|#{sel} where #{qw}|]

-- |Construct a query of the shape "select ... from ... where ..."
-- The point of the 'ResultShape' constraint is to help with inferring the @d@ type from the @result@ type so no manual
-- type application is necessary.
selectWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
selectWhere table@(QueryTable (Table _ row _ _) params _) =
  query (selectWhereSql table) row params

anyWhereSql ::
  QueryTable query d ->
  SqlCode
anyWhereSql (QueryTable (Table (Column _ (fromFragment -> SqlCode from) _ _ _) _ _ _) _ (Where (SqlCode qw))) =
  SqlCode [qt|select 1 #{from} where #{qw} limit 1|]

anyWhere ::
  QueryTable query d ->
  Statement query Bool
anyWhere table@(QueryTable _ params _) =
  isJust <$> query (anyWhereSql table) (Decoders.column (nullable int8)) params

insert ::
  Table d ->
  Statement d ()
insert Table {_structure, _params} =
  query (Query.insert _structure) unit _params

upsertSql ::
  Column ->
  SqlCode
upsertSql table =
  SqlCode [qt|#{ins} #{conflict}|]
  where
    SqlCode conflict =
      conflictFragment table st
    SqlCode ins =
      Query.insert table
    st =
      Query.set table

upsert ::
  Table d ->
  Statement d ()
upsert Table {_structure, _params} =
  query (upsertSql _structure) unit _params

deleteWhereSql ::
  Table d ->
  SqlCode ->
  SqlCode
deleteWhereSql Table {_structure} (SqlCode qw) =
  [qt|#{del}#{qwFragment} returning #{cols}|]
  where
    qwFragment =
      if Text.null qw
      then "" :: Text
      else [qt| where #{qw}|]
    SqlCode del =
      deleteSql _structure
    cols =
      commaColumns (baseColumns _structure)

deleteWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
deleteWhere (QueryTable table@Table {_row} params (Where qw)) =
  query (deleteWhereSql table qw) _row params

deleteAll ::
  Table d ->
  Statement () [d]
deleteAll table@(Table _ row _ _) =
  query (deleteWhereSql table "") row noParams

updateSql ::
  QueryTable query d ->
  SqlCode
updateSql table@(QueryTable (Table structure _ _ _) _ (Where (SqlCode qw))) =
  SqlCode [qt|#{upd} #{st}#{qwFragment}|]
  where
    qwFragment =
      if Text.null qw
      then "" :: Text
      else [qt| where #{qw}|]
    SqlCode upd =
      Query.update table
    SqlCode st =
      Query.setPartial structure

update ::
  QueryTable query d ->
  Statement (PartialFields d) ()
update qtable@(QueryTable Table {_partialParams} _ _) =
  query (updateSql qtable) unit _partialParams

createTableSql ::
  Column ->
  SqlCode
createTableSql column@(Column _ (Selector selector) _ _ _) =
  SqlCode [qt|create table #{selector} (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (unSqlCode . columnSpec <$> baseColumns column))

createTable ::
  Column ->
  Statement () ()
createTable =
  plain . createTableSql

createCtorTypeSql :: Column -> SqlCode
createCtorTypeSql column =
  [qt|create type #{quotedName column} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (formattedColumn <$> baseColumns column))
    formattedColumn col =
      [qt|#{quotedName col} #{dquote (Column._tpe col)}|]

createCtorType ::
  Column ->
  Statement () ()
createCtorType =
  plain . createCtorTypeSql

createProdTypeSql ::
  Selector ->
  [Column] ->
  SqlCode
createProdTypeSql (Selector prodName) columns =
  [qt|create type #{prodName} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (formattedColumn <$> columns)
    formattedColumn (Column (Name name) _ tpe _ _) =
      [qt|#{name} #{tpe}|]

createProdType ::
  Selector ->
  [Column] ->
  Statement () ()
createProdType =
  plain .: createProdTypeSql

createSumTypeSql :: Column -> SqlCode
-- createSumTypeSql (Column (quotedName -> name) (Column indexName indexType _ _) columns) =
createSumTypeSql column =
  [qt|create type #{quotedName column} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated ([qt|#{dquote "sum_index"} bigint|] : toList (formattedColumn <$> baseColumns column))
    formattedColumn (quotedName -> n) =
      [qt|#{n} #{n}|]

createSumType ::
  Column ->
  Statement () ()
createSumType =
  plain . createSumTypeSql

dropTypeSql ::
  Text ->
  SqlCode
dropTypeSql (dquote -> name) =
   SqlCode [qt|drop type if exists #{name} cascade|]

dropType ::
  Text ->
  Statement () ()
dropType =
  plain . dropTypeSql

dropTableSql ::
  Name ->
  SqlCode
dropTableSql (Name (dquote -> name)) =
   SqlCode [qt|drop table if exists #{name}|]

dropTable ::
  Name ->
  Statement () ()
dropTable =
  plain . dropTableSql

alterSql ::
  Selector ->
  NonEmpty Column ->
  SqlCode
alterSql (alterFragment -> SqlCode alter') (toList -> columns) =
  SqlCode [qt|#{alter'} #{colAdds}|]
  where
    SqlCode colAdds =
      commaSeparatedSql (addColumnFragment <$> columns)

alter ::
  Selector ->
  NonEmpty Column ->
  Statement () ()
alter =
  plain .: alterSql

createDbSql ::
  DbName ->
  SqlCode
createDbSql (DbName (dquote -> name)) =
  SqlCode [qt|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  plain . createDbSql

dropDbSql ::
  DbName ->
  SqlCode
dropDbSql (DbName (dquote -> name)) =
  SqlCode [qt|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  plain . dropDbSql
