module Sqel.Statement where

import Hasql.Decoders (Row, noResult)
import Hasql.Encoders (Params)
import Hasql.Statement (Statement (Statement))

import Sqel.Data.Codec (Encoder (Encoder))
import Sqel.Data.ColumnOptions (ColumnOptions (ColumnOptions))
import Sqel.Data.PgType (PgColumnName (PgColumnName))
import qualified Sqel.Data.PgType as PgTable
import Sqel.Data.PgType (ColumnType (ColumnPrim), PgColumns (PgColumns), PgTable (PgTable))
import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.ResultShape (ResultShape (resultShape))
import Sqel.Sql.CommaSep (CommaSep (CommaSep))
import Sqel.Sql.Delete (Delete (Delete))
import Sqel.Sql.Insert (Insert (Insert))
import Sqel.Sql.Returning (Returning (Returning))
import Sqel.Sql.Select (Select (Select))
import Sqel.Sql.Update (Update (Update))
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

qStatement ::
  ∀ q a result .
  ResultShape a result =>
  QuerySchema q a ->
  TableSchema a ->
  Statement q result
qStatement (QuerySchema query (Encoder qp _)) (TableSchema col row _) =
  prepared [sql|##{Select col} ##{query}|] row qp

dStatement ::
  ResultShape a result =>
  QuerySchema q a ->
  TableSchema a ->
  Statement q result
dStatement (QuerySchema query (Encoder qp _)) (TableSchema col row _) =
  prepared [sql|##{Delete col} ##{query} ##{Returning col}|] row qp

iStatement ::
  TableSchema a ->
  Statement a ()
iStatement (TableSchema col _ params) =
  prepared [sql|##{Insert col}|] unit params

uniqueOrPrimary :: ColumnOptions -> Bool
uniqueOrPrimary (ColumnOptions u _ p) =
  u || p

uniqueColumn :: (PgColumnName, ColumnType) -> Maybe Selector
uniqueColumn = \case
  (PgColumnName name, ColumnPrim _ (uniqueOrPrimary -> True)) ->
    Just (Selector (Sql (dquote name)))
  _ ->
    Nothing

pattern UniqueName :: Selector -> (PgColumnName, ColumnType)
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

uStatement ::
  TableSchema a ->
  Statement a ()
uStatement (TableSchema tab _ params) =
  prepared (upsertSql tab) unit params
