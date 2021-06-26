module Polysemy.Hasql.Table.Query.Update where

import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreePrim (..), foldTree)
import Polysemy.Db.Tree.Partial (PartialTree, Partially)

import Polysemy.Hasql.Data.DbType (Selector (Selector))
import Polysemy.Hasql.Data.Table (Table, selector, structure)
import Polysemy.Hasql.Data.Where (Where (Where))
import Polysemy.Hasql.DbType (baseColumns)
import Polysemy.Hasql.Table.Query.Text (commaColumns)
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))

newtype PartialSql =
  PartialSql { unPartialSql :: Snippet }
  deriving newtype (Semigroup, Monoid)

type BuildPartialSql d tree =
  (
    Partially d tree,
    FoldTree 'True () PartialField [PartialSql] tree
  )

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

update ::
  BuildPartialSql d tree =>
  Table d ->
  Maybe (Where query d) ->
  query ->
  PartialTree tree ->
  Snippet
update table qWhere q tree =
  sql [text|update #{sel} set |] <>
  commaSeparatedSnippet (unPartialSql <$> (foldTree @'True tree)) <>
  foldMap whereSnippet qWhere <>
  " returning " <>
  sql (encodeUtf8 cols)
  where
    Selector sel =
      table ^. selector
    whereSnippet (Where _ qw) =
      " where " <> qw q
    cols =
      commaColumns (baseColumns (table ^. structure))
