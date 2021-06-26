module Polysemy.Hasql.Statement where

import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, int8, noResult, nullable)
import Hasql.DynamicStatements.Snippet (Snippet)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Encoders (Params, noParams)
import Hasql.Statement (Statement (Statement))
import Polysemy.Db.Data.DbName (DbName (DbName))
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Db.Tree.Partial (PartialTree)

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column (Column), Name (Name), Selector, TypeName ())
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable), qwhere)
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode, unSqlCode))
import Polysemy.Hasql.Data.Table (Table (Table, _params, _row, _structure))
import Polysemy.Hasql.Data.Where (Where (Where))
import Polysemy.Hasql.DbType (baseColumns, columnSpec, quotedName, typeName)
import Polysemy.Hasql.Table.Query.Delete (deleteSql)
import Polysemy.Hasql.Table.Query.Fragment (addColumnFragment, alterFragment, conflictFragment, fromFragment)
import qualified Polysemy.Hasql.Table.Query.Insert as Query (insert)
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import qualified Polysemy.Hasql.Table.Query.Set as Query (set)
import Polysemy.Hasql.Table.Query.Text (commaColumns, commaSeparated, commaSeparatedSql)
import qualified Polysemy.Hasql.Table.Query.Update as Query
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.ResultShape (ResultShape (resultShape))

statement ::
  ResultShape d result =>
  Bool ->
  SqlCode ->
  Row d ->
  Params p ->
  Statement p result
statement prep (SqlCode sql) row params =
  Statement (encodeUtf8 sql) params (resultShape row) prep

unprepared ::
  ResultShape d result =>
  SqlCode ->
  Row d ->
  Params p ->
  Statement p result
unprepared =
  statement False

prepared ::
  ResultShape d result =>
  SqlCode ->
  Row d ->
  Params p ->
  Statement p result
prepared =
  statement True

plain :: SqlCode -> Statement () ()
plain (SqlCode sql) =
  Statement (encodeUtf8 sql) mempty noResult False

select ::
  ∀ d .
  Column ->
  Row d ->
  Statement () [d]
select table row =
  prepared (selectColumns table) row noParams

selectWhereSql ::
  QueryTable query d ->
  SqlCode
selectWhereSql (QueryTable (Table (selectColumns -> SqlCode sel) _ _) _ (Where (SqlCode qw) _)) =
  SqlCode [text|#{sel} where #{qw}|]

-- |Construct a query of the shape "select ... from ... where ..."
-- The point of the 'ResultShape' constraint is to help with inferring the @d@ type from the @result@ type so no manual
-- type application is necessary.
selectWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
selectWhere table@(QueryTable (Table _ row _) params _) =
  prepared (selectWhereSql table) row params

anyWhereSql ::
  QueryTable query d ->
  SqlCode
anyWhereSql table =
  SqlCode [text|select 1 #{from} where #{qw} limit 1|]
  where
    Where (SqlCode qw) _ =
      table ^. qwhere
    SqlCode from =
      fromFragment (table ^. QueryTable.structure . Column.selector)

anyWhere ::
  QueryTable query d ->
  Statement query Bool
anyWhere table@(QueryTable {_qparams}) =
  isJust <$> prepared (anyWhereSql table) (Decoders.column (nullable int8)) _qparams

insert ::
  Table d ->
  Statement d ()
insert Table {_structure, _params} =
  prepared (Query.insert _structure) unit _params

upsertSql ::
  Column ->
  SqlCode
upsertSql table =
  SqlCode [text|#{ins} #{conflict}|]
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
  prepared (upsertSql _structure) unit _params

deleteWhereSql ::
  Table d ->
  SqlCode ->
  SqlCode
deleteWhereSql Table {_structure} (SqlCode qw) =
  [text|#{del}#{qwFragment} returning #{cols}|]
  where
    qwFragment =
      if Text.null qw
      then "" :: Text
      else [text| where #{qw}|]
    SqlCode del =
      deleteSql _structure
    cols =
      commaColumns (baseColumns _structure)

deleteWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
deleteWhere QueryTable {_table = table@Table {_row}, _qparams, _qwhere = Where qw _} =
  prepared (deleteWhereSql table qw) _row _qparams

deleteAll ::
  Table d ->
  Statement () [d]
deleteAll table@(Table _ row _) =
  prepared (deleteWhereSql table "") row noParams

updateSql ::
  BuildPartialSql d tree =>
  Table d ->
  Maybe (Where query d) ->
  query ->
  PartialTree tree ->
  Snippet
updateSql table qw q tree =
  Query.update table qw q tree

update ::
  BuildPartialSql d tree =>
  QueryTable query d ->
  query ->
  PartialTree tree ->
  Statement () (Maybe d)
update (QueryTable tbl@Table {_row} _ qw) q t =
  dynamicallyParameterized (updateSql tbl (Just qw) q t) (resultShape _row) False

updateSingle ::
  BuildPartialSql d tree =>
  Table d ->
  PartialTree tree ->
  Statement () (Maybe d)
updateSingle tbl@Table {_row} t =
  dynamicallyParameterized (updateSql tbl Nothing () t) (resultShape _row) False

createTableSql ::
  Column ->
  SqlCode
createTableSql column@(Column _ _ _ _ _) =
  SqlCode [text|create table #{quotedName column} (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (unSqlCode . columnSpec <$> baseColumns column))

createTable ::
  Column ->
  Statement () ()
createTable =
  plain . createTableSql

createCtorTypeSql :: Column -> SqlCode
createCtorTypeSql column@(Column _ _ tpe _ _) =
  [text|create type #{typeName tpe} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (formattedColumn <$> baseColumns column))
    formattedColumn col@(Column _ _ t _ _) =
      [text|#{quotedName col} #{typeName t}|]

createCtorType ::
  Column ->
  Statement () ()
createCtorType =
  plain . createCtorTypeSql

createProdTypeSql ::
  TypeName ->
  [Column] ->
  SqlCode
createProdTypeSql prodName columns =
  [text|create type #{typeName prodName} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (formattedColumn <$> columns)
    formattedColumn column@(Column _ _ tpe _ _) =
      [text|#{quotedName column} #{typeName tpe}|]

createProdType ::
  TypeName ->
  [Column] ->
  Statement () ()
createProdType =
  plain .: createProdTypeSql

dropTypeSql ::
  Text ->
  SqlCode
dropTypeSql (dquote -> name) =
   SqlCode [text|drop type if exists #{name} cascade|]

dropType ::
  Text ->
  Statement () ()
dropType =
  plain . dropTypeSql

dropTableSql ::
  Name ->
  SqlCode
dropTableSql (Name (dquote -> name)) =
   SqlCode [text|drop table if exists #{name}|]

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
  SqlCode [text|#{alter'} #{colAdds}|]
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
  SqlCode [text|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  plain . createDbSql

dropDbSql ::
  DbName ->
  SqlCode
dropDbSql (DbName (dquote -> name)) =
  SqlCode [text|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  plain . dropDbSql
