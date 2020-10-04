module Polysemy.Hasql.Table.Query.Select where

import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(..))
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)

assembleVariant :: Text -> TableStructure -> [Text]
assembleVariant name (TableStructure (TableName variantName) columns) =
  column <$> columns
  where
    column (Column colName _ _ _) =
      [qt|(#{dquote name}).#{dquote variantName}.#{dquote colName}|]

assembleComposite :: Text -> CompositeType -> [Text]
assembleComposite name (CompositeType _ _ variants) =
  pure [qt|(#{dquote name}).sum_index|] <> (variants >>= assembleVariant name)

assembleColumns :: [Column] -> [Text]
assembleColumns =
  (>>= column)
  where
    column = \case
      Column name _ _ (Just composite) ->
        assembleComposite name composite
      Column name _ _ Nothing ->
        [dquote name]

selectColumns ::
  TableStructure ->
  SqlCode
selectColumns (TableStructure (fromFragment -> SqlCode from) (assembleColumns -> columns)) =
  SqlCode [qt|select #{commaSeparated columns} #{from}|]
