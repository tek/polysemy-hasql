module Polysemy.Hasql.Table.Query.Update where

import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreePrim(..), foldTree)
import Polysemy.Db.Tree.Partial (PartialTree)

import Polysemy.Hasql.Data.DbType (Selector(Selector))
import Polysemy.Hasql.Data.QueryTable (QueryTable, qwhere, selector, structure)
import Polysemy.Hasql.Data.Where (Where(Where))
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))
import Polysemy.Hasql.Table.Query.Text (commaColumns)
import Polysemy.Hasql.DbType (baseColumns)

newtype PartialSql =
  PartialSql { unPartialSql :: Snippet }
  deriving newtype (Semigroup, Monoid)

commaSeparatedSnippet ::
  [Snippet] ->
  Snippet
commaSeparatedSnippet =
  mconcat . intersperse ", "

instance (
    QueryValueNoN effs d
  ) => FoldTreePrim root () PartialField [PartialSql] name effs d where
  foldTreePrim = \case
    PartialField.Keep -> mempty
    PartialField.Update name value ->
      [PartialSql (sql (encodeUtf8 (dquote name)) <> " = " <> encoderAndParam (queryValueNoN @effs @d) value)]

-- TODO where fragment needs to be constructed with explicit values, the regular derivation produces a fragment for a
-- prepared statement
update ::
  FoldTree 'True () PartialField [PartialSql] tree =>
  QueryTable query d ->
  query ->
  PartialTree tree ->
  Snippet
update table q tree =
  sql [text|update #{sel} set |] <>
  commaSeparatedSnippet (unPartialSql <$> (foldTree @'True tree)) <>
  " where " <>
  qw q <>
  " returning " <>
  sql (encodeUtf8 cols)
  where
    Selector sel =
      table ^. selector
    Where _ qw =
      table ^. qwhere
    cols =
      commaColumns (baseColumns (table ^. structure))
