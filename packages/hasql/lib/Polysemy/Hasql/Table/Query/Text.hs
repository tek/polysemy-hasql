module Polysemy.Hasql.Table.Query.Text where

import qualified Data.Text as Text

import qualified Polysemy.Db.Data.TableStructure as Column
import Polysemy.Db.Data.TableStructure (Column)
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode, unSqlCode))

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
  [Column] ->
  Text
commaColumns columns =
  commaFields (Column.columnName <$> columns)

commaSeparatedSql ::
  Functor t =>
  Foldable t =>
  t SqlCode ->
  SqlCode
commaSeparatedSql =
  SqlCode . commaSeparated . fmap unSqlCode
