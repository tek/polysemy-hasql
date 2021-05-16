module Polysemy.Hasql.Table.Query.Update where

import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Text.Quote (dquote)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreePrim(..), foldTree)
import Polysemy.Db.Tree.Partial (PartialTree)

import Polysemy.Hasql.Data.DbType (Selector(Selector))
import Polysemy.Hasql.Data.QueryTable (QueryTable, selector)
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))

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
  ) => FoldTreePrim () PartialField [PartialSql] effs d where
  foldTreePrim = \case
    PartialField.Keep -> mempty
    PartialField.Update name value ->
      [PartialSql (sql (encodeUtf8 (dquote name)) <> " = " <> encoderAndParam (queryValueNoN @effs @d) value)]

update ::
  FoldTree () PartialField [PartialSql] tree =>
  QueryTable query d ->
  q ->
  PartialTree tree ->
  Snippet
update table _ tree =
  sql [text|update #{sel} set |] <> (commaSeparatedSnippet (unPartialSql <$> (foldTree tree)))
  where
    Selector sel =
      table ^. selector
