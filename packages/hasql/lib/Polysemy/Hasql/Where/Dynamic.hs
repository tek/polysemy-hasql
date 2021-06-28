module Polysemy.Hasql.Where.Dynamic where

import Generics.SOP (I (I))
import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Text.DbIdentifier (dbSymbolBS)
import Polysemy.Db.Tree.Fold (FoldTree (foldTree), FoldTreePrim (..))

import Polysemy.Hasql.Table.Query.Update (commaSeparatedSnippet)
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))
import Polysemy.Hasql.Tree.Value (DbValueQueryTree, dbValueQueryTree)

newtype DynQuerySql =
  DynQuerySql { unDynQuerySql :: Snippet }
  deriving newtype (Semigroup, Monoid)

field ::
  ∀ name effs q .
  KnownSymbol name =>
  QueryValueNoN effs q =>
  q ->
  Snippet
field q =
  sql (dbSymbolBS @name) <> " = " <> encoderAndParam (queryValueNoN @effs) q

instance {-# overlappable #-} (
    KnownSymbol name,
    QueryValueNoN effs d
  ) => FoldTreePrim root () I [DynQuerySql] ('NamedField name) effs d where
  foldTreePrim (I q) =
    [DynQuerySql (field @name @effs q)]

instance FoldTreePrim root () I [DynQuerySql] ('NamedField name) effs () where
  foldTreePrim (I _) =
    []

class DynamicQuery (rep :: Type) (q :: Type) (d :: Type) where
  dynamicQuery :: q -> Snippet

instance (
    DbValueQueryTree rep q d tree,
    FoldTree 'True () I [DynQuerySql] tree
  ) => DynamicQuery rep q d where
    dynamicQuery =
      commaSeparatedSnippet . fmap unDynQuerySql . foldTree @'True . dbValueQueryTree @rep @q @d
