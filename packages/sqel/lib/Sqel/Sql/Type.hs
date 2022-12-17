module Sqel.Sql.Type where

import qualified Exon

import qualified Sqel.Data.PgType as PgTable
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Sql.CommaSep (CommaSep (CommaSep))
import Sqel.Text.Quote (dquote)

columnSpec ::
  PgColumnName ->
  ColumnType ->
  Sql
columnSpec (PgColumnName name) = \case
  ColumnPrim (PgPrimName tpe) _ (Exon.intercalate " " -> params) ->
    [sql|##{dquote name} ##{tpe} #{params}|]
  ColumnComp (PgTypeRef tpe) ->
    [sql|##{dquote name} ##{tpe}|]

typeColumnSpec ::
  PgColumnName ->
  ColumnType ->
  Sql
typeColumnSpec (PgColumnName name) = \case
  ColumnPrim (PgPrimName tpe) _ _ ->
    [sql|##{dquote name} ##{tpe}|]
  ColumnComp (PgTypeRef tpe) ->
    [sql|##{dquote name} ##{tpe}|]

-- TODO this should use PgColumns, not PgStructure
createTable ::
  PgTable a ->
  Sql
createTable PgTable {name, columns = PgColumns cols} =
  [sql|create table ##{name} (##{CommaSep formattedColumns})|]
  where
    formattedColumns = toList (uncurry columnSpec <$> cols)

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
    formattedColumns = toList (uncurry typeColumnSpec <$> cols)
