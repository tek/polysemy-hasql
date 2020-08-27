module Polysemy.Db.Table.Query.Text where

import qualified Data.Text as Text

import Polysemy.Db.Data.Columns (Columns(Columns))
import qualified Polysemy.Db.Data.Columns as Column
import Polysemy.Db.Data.SqlCode (SqlCode(SqlCode, unSqlCode))
import Polysemy.Db.Text.Quote (dquote)

commaSeparated ::
  Foldable t =>
  t Text ->
  Text
commaSeparated =
  Text.intercalate ", " . toList

commaFields ::
  Foldable t =>
  t Text ->
  Text
commaFields =
  commaSeparated . fmap dquote . toList

commaColumns ::
  Columns ->
  Text
commaColumns (Columns columns) =
  commaFields (Column.name <$> columns)

commaSeparatedSql ::
  Foldable t =>
  t SqlCode ->
  SqlCode
commaSeparatedSql =
  SqlCode . commaSeparated . fmap unSqlCode . toList
