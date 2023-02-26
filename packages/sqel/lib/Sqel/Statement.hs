module Sqel.Statement where

import Exon (exon)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, noResult)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Hasql.Statement (Statement (Statement))

import Sqel.Data.Codec (Encoder (Encoder))
import qualified Sqel.Data.PgType as PgTable
import Sqel.Data.PgType (
  ColumnType (ColumnPrim),
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgTable (PgTable),
  )
import Sqel.Data.Projection (Projection)
import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Data.SqlFragment (
  CommaSep (CommaSep),
  Delete (Delete),
  Insert (Insert),
  Returning (Returning),
  Update (Update),
  )
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.ResultShape (ResultShape (resultShape))
import qualified Sqel.Sql.Select as Sql
import qualified Sqel.Sql.Type as Sql
import Sqel.Text.Quote (dquote)

statement ::
  ResultShape d result =>
  Bool ->
  Sql ->
  Row d ->
  Params p ->
  Statement p result
statement prep (Sql s) row params =
  Statement (encodeUtf8 s) params (resultShape row) prep

unprepared ::
  ∀ result d p .
  ResultShape d result =>
  Sql ->
  Row d ->
  Params p ->
  Statement p result
unprepared =
  statement False

prepared ::
  ResultShape d result =>
  Sql ->
  Row d ->
  Params p ->
  Statement p result
prepared =
  statement True

plain :: Sql -> Statement () ()
plain s =
  Statement (encodeUtf8 s) mempty noResult False

selectWhere ::
  ∀ result proj q table .
  ResultShape proj result =>
  QuerySchema q table ->
  Projection proj table ->
  Statement q result
selectWhere q@(QuerySchema _ (Encoder qp _)) t =
  prepared (Sql.selectWhereGen q t) (t ^. #decoder) qp

delete ::
  ResultShape a result =>
  QuerySchema q a ->
  TableSchema a ->
  Statement q result
delete (QuerySchema query (Encoder qp _)) (TableSchema col row _) =
  prepared [sql|##{Delete col} ##{query} ##{Returning col}|] row qp

insert ::
  TableSchema a ->
  Statement a ()
insert (TableSchema col _ params) =
  prepared [sql|##{Insert col}|] unit params

uniqueColumn :: PgColumn -> Maybe Selector
uniqueColumn = \case
  PgColumn (PgColumnName name) (ColumnPrim _ True _) ->
    Just (Selector (Sql (dquote name)))
  _ ->
    Nothing

pattern UniqueName :: Selector -> PgColumn
pattern UniqueName sel <- (uniqueColumn -> Just sel)

conflictFragment ::
  PgTable a ->
  Sql
conflictFragment table@PgTable {columns = PgColumns columns} =
  format uniques
  where
    format Nothing =
      ""
    format (Just cols) =
      [sql|on conflict (##{CommaSep (toList cols)}) do ##{Update table}|]
    uniques =
      nonEmpty [n | UniqueName (Selector n) <- columns]

upsertSql :: PgTable a -> Sql
upsertSql tab =
  [sql|##{Insert tab} #{conflict}|]
  where
    conflict = conflictFragment tab

upsert ::
  TableSchema a ->
  Statement a ()
upsert (TableSchema tab _ params) =
  prepared (upsertSql tab) unit params

dbColumns ::
  Sql ->
  Statement Text [(Text, Text, Text)]
dbColumns code =
  prepared code decoder encoder
  where
    decoder =
      (,,) <$> text' <*> text' <*> text'
    text' =
      Decoders.column (Decoders.nonNullable Decoders.text)
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

tableColumnsSql :: Sql
tableColumnsSql =
  [exon|select "column_name", "data_type", "udt_name" from information_schema.columns where "table_name" = $1|]

typeColumnsSql :: Sql
typeColumnsSql =
  [exon|select "attribute_name", "data_type", "attribute_udt_name" from information_schema.attributes where "udt_name" = $1|]

createTable :: PgTable a -> Statement () ()
createTable table =
  unprepared (Sql.createTable table) unit mempty
