module Sqel.Sql.Type where

import qualified Exon

import qualified Sqel.Data.PgType as PgTable
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.SqlFragment (CommaSep (CommaSep))
import Sqel.Text.Quote (dquote)

-- TODO why is unique not used?
columnSpec ::
  PgColumn ->
  Sql
columnSpec = \case
  PgColumn (PgColumnName name) (ColumnPrim (PgPrimName tpe) _ (Exon.intercalate " " -> params)) ->
    [sql|##{dquote name} ##{tpe} #{params}|]
  PgColumn (PgColumnName name) (ColumnComp (PgTypeRef tpe) _ (Exon.intercalate " " -> params)) ->
    [sql|##{dquote name} ##{tpe} #{params}|]

typeColumnSpec ::
  PgColumn ->
  Sql
typeColumnSpec = \case
  PgColumn (PgColumnName name) (ColumnPrim (PgPrimName tpe) _ _) ->
    [sql|##{dquote name} ##{tpe}|]
  PgColumn (PgColumnName name) (ColumnComp (PgTypeRef tpe) _ _) ->
    [sql|##{dquote name} ##{tpe}|]

createTable ::
  PgTable a ->
  Sql
createTable PgTable {name, columns = PgColumns cols} =
  [sql|create table ##{name} (##{CommaSep formattedColumns})|]
  where
    formattedColumns = toList (columnSpec <$> cols)

dropTable ::
  PgTableName ->
  Sql
dropTable name =
   [sql|drop table if exists ##{name}|]

-- TODO make PgTypeRef et al quote automatically by using @ToSegment@
createProdType ::
  PgComposite ->
  Sql
createProdType (PgComposite name (PgColumns cols)) =
  [sql|create type ##{name} as (##{CommaSep formattedColumns})|]
  where
    formattedColumns = toList (typeColumnSpec <$> cols)
