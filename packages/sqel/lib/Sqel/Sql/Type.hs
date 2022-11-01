module Sqel.Sql.Type where

import qualified Sqel.Data.ColumnOptions as ColumnOptions
import Sqel.Data.PgType (PgColumnName (PgColumnName), PgPrimName (PgPrimName))
import qualified Sqel.Data.PgType as PgTable
import Sqel.Data.PgType (
  PgStructure (PgStructure),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  StructureType (StructureComp, StructurePrim),
  )
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Sql.CommaSep (CommaSep (CommaSep))
import Sqel.Text.Quote (dquote)

columnSpec ::
  PgColumnName ->
  StructureType ->
  Sql
columnSpec (PgColumnName name) = \case
  StructurePrim (PgPrimName tpe) (ColumnOptions.format -> params) ->
    [sql|##{dquote name} ##{tpe} #{params}|]
  StructureComp (PgTypeRef tpe) _ ->
    [sql|##{dquote name} ##{tpe}|]

typeColumnSpec ::
  PgColumnName ->
  StructureType ->
  Sql
typeColumnSpec (PgColumnName name) = \case
  StructurePrim (PgPrimName tpe) _ ->
    [sql|##{dquote name} ##{tpe}|]
  StructureComp (PgTypeRef tpe) _ ->
    [sql|##{dquote name} ##{tpe}|]

-- TODO this should use PgColumns, not PgStructure
createTable ::
  PgTable a ->
  Sql
createTable PgTable {name, structure = PgStructure cols} =
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
  PgTypeRef ->
  PgStructure ->
  Sql
createProdType name (PgStructure cols) =
  [sql|create type ##{name} as (##{CommaSep formattedColumns})|]
  where
    formattedColumns = toList (uncurry typeColumnSpec <$> cols)
