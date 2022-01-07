module Polysemy.Hasql.Statement where

import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, int8, noResult, nullable)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Encoders (Params, noParams)
import Hasql.Statement (Statement (Statement))
import Polysemy.Db.Data.DbName (DbName)
import Polysemy.Db.Data.PartialField (PartialField, PartialTree)
import Polysemy.Db.Tree.FoldMap (FoldMapTree)

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column (Column), Name (Name), Selector, TypeName)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable), qwhere)
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import Polysemy.Hasql.Data.Table (Table (Table, _params, _row, _structure))
import Polysemy.Hasql.Data.Where (Where (Where))
import Polysemy.Hasql.DbType (baseColumns, columnSpec, quotedName, typeName)
import Polysemy.Hasql.Table.Query.Delete (deleteSql)
import Polysemy.Hasql.Table.Query.Fragment (
  addColumnFragment,
  alterFragment,
  conflictFragment,
  fromFragment,
  selectFragment,
  whereFragment,
  )
import qualified Polysemy.Hasql.Table.Query.Insert as Query (insert)
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import qualified Polysemy.Hasql.Table.Query.Set as Query (set)
import Polysemy.Hasql.Table.Query.Text (commaSeparated, quoteName, sqlQuote)
import qualified Polysemy.Hasql.Table.Query.Update as Query
import Polysemy.Hasql.Table.Query.Update (PartialSql)
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
plain sql =
  Statement (encodeUtf8 sql) mempty noResult False

selectSql ::
  Column ->
  SqlCode
selectSql column@Column {_selector} =
  [exon|#{sel} #{from}|]
  where
    sel =
      selectFragment column
    from =
      fromFragment _selector

select ::
  ∀ d result .
  ResultShape d result =>
  Column ->
  Row d ->
  Statement () result
select column row =
  prepared (selectSql column) row noParams

selectWhereSql ::
  QueryTable query d ->
  SqlCode
selectWhereSql (QueryTable (Table column _ _) _ (whereFragment -> wh)) =
  [exon|#{sel} #{wh}|]
  where
    sel =
      selectSql column

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
  [exon|select 1 #{from} where #{qw} limit 1|]
  where
    Where qw _ =
      table ^. qwhere
    from =
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
  [exon|#{ins} #{conflict}|]
  where
    conflict =
      conflictFragment table st
    ins =
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
  Where q d ->
  SqlCode
deleteWhereSql Table {_structure} where' =
  [exon|#{del} #{wh} returning #{cols}|]
  where
    wh =
      whereFragment where'
    del =
      deleteSql _structure
    cols =
      selectColumns _structure

deleteWhere ::
  ResultShape d result =>
  QueryTable query d ->
  Statement query result
deleteWhere QueryTable {_table = table@Table {_row}, _qparams, _qwhere} =
  prepared (deleteWhereSql table _qwhere) _row _qparams

deleteAll ::
  Table d ->
  Statement () [d]
deleteAll table@(Table _ row _) =
  prepared (deleteWhereSql table mempty) row noParams

update ::
  FoldMapTree 'True () PartialField [PartialSql] tree =>
  QueryTable query d ->
  query ->
  PartialTree tree ->
  Statement () (Maybe d)
update (QueryTable tbl@Table {_row} _ qw) q t =
  dynamicallyParameterized (Query.update tbl (Just qw) q t) (resultShape _row) False

updateSingle ::
  FoldMapTree 'True () PartialField [PartialSql] tree =>
  Table d ->
  PartialTree tree ->
  Statement () (Maybe d)
updateSingle tbl@Table {_row} t =
  dynamicallyParameterized (Query.update tbl Nothing () t) (resultShape _row) False

createTableSql ::
  Column ->
  SqlCode
createTableSql column =
  [exon|create table #{quotedName column} (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (columnSpec <$> baseColumns column))

createTable ::
  Column ->
  Statement () ()
createTable =
  plain . createTableSql

createCtorTypeSql :: Column -> SqlCode
createCtorTypeSql column@(Column _ _ tpe _ _) =
  [exon|create type #{typeName tpe} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (toList (formattedColumn <$> baseColumns column))
    formattedColumn col@(Column _ _ t _ _) =
      [exon|#{quotedName col} #{typeName t}|]

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
  [exon|create type #{typeName prodName} as (#{formattedColumns})|]
  where
    formattedColumns =
      commaSeparated (formattedColumn <$> columns)
    formattedColumn column@(Column _ _ tpe _ _) =
      [exon|#{quotedName column} #{typeName tpe}|]

createProdType ::
  TypeName ->
  [Column] ->
  Statement () ()
createProdType =
  plain .: createProdTypeSql

dropTypeSql ::
  Text ->
  SqlCode
dropTypeSql (sqlQuote -> name) =
   [exon|drop type if exists #{name} cascade|]

dropType ::
  Text ->
  Statement () ()
dropType =
  plain . dropTypeSql

dropTableSql ::
  Name ->
  SqlCode
dropTableSql (Name (sqlQuote -> name)) =
   [exon|drop table if exists #{name}|]

dropTable ::
  Name ->
  Statement () ()
dropTable =
  plain . dropTableSql

alterSql ::
  Selector ->
  NonEmpty Column ->
  SqlCode
alterSql (alterFragment -> alter') (toList -> columns) =
  [exon|#{alter'} #{colAdds}|]
  where
    colAdds =
      commaSeparated (addColumnFragment <$> columns)

alter ::
  Selector ->
  NonEmpty Column ->
  Statement () ()
alter =
  plain .: alterSql

createDbSql ::
  DbName ->
  SqlCode
createDbSql (quoteName -> name) =
  [exon|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  plain . createDbSql

dropDbSql ::
  DbName ->
  SqlCode
dropDbSql (quoteName -> name) =
  [exon|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  plain . dropDbSql
