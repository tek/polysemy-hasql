module Polysemy.Hasql.Where.Dynamic where

import Fcf (FromMaybe, Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (I (I))
import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import Polysemy.Db.Data.ColumnPrefix (ColumnPrefix (InitPrefix), addPrefix, prefixed, promotePrefix)
import Polysemy.Db.Data.FieldId (FieldId (NamedField), FieldIdText (fieldIdText))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Fold (FoldTree (foldTree), FoldTreeLocal (foldTreeLocal), FoldTreeLocalCon (foldTreeLocalCon), FoldTreePrim (..))
import Polysemy.Db.Tree.QueryName (FindPrimQuery)

import Polysemy.Hasql.Data.DbType (Selector (Selector), textSelector)
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))
import Polysemy.Hasql.Tree.Value (DbValueQueryTree, dbValueQueryTree)

newtype DynQuerySql =
  DynQuerySql { unDynQuerySql :: Snippet }
  deriving newtype (Semigroup, Monoid)

field ::
  ∀ effs q .
  QueryValueNoN effs q =>
  Selector ->
  q ->
  Snippet
field (Selector name) q =
  sql (encodeUtf8 name) <> " = " <> encoderAndParam (queryValueNoN @effs) q

instance {-# overlappable #-} (
    FieldIdText name,
    QueryValueNoN effs d
  ) => FoldTreePrim root () I ColumnPrefix [DynQuerySql] name effs d where
  foldTreePrim prefix (I q) =
    [DynQuerySql (field @effs selector q)]
    where
      selector =
        textSelector (prefixed name prefix)
      name =
        fieldIdText @name

instance FoldTreePrim root () I ColumnPrefix [DynQuerySql] ('NamedField name) effs () where
  foldTreePrim _ (I _) =
    []

instance (
    field ~ (FromMaybe name @@ (FMap (Pure1 'NamedField) @@ FindPrimQuery effs)),
    FieldIdText field
  ) => FoldTreeLocal () I ColumnPrefix [DynQuerySql] name effs node where
  foldTreeLocal =
    addPrefix name
    where
      name =
        fieldIdText @field

instance (
    FieldIdText name
  ) => FoldTreeLocalCon () I ColumnPrefix [DynQuerySql] name effs ('Kind.Con num cname sub) where
  foldTreeLocalCon =
    addPrefix name
    where
      name =
        fieldIdText @name

instance FoldTreeLocalCon () I ColumnPrefix [DynQuerySql] name effs ('Kind.ConUna num cname sub) where
  foldTreeLocalCon =
    promotePrefix

class DynamicQuery (rep :: Type) (q :: Type) (d :: Type) where
  dynamicQuery :: q -> Snippet

instance (
    DbValueQueryTree rep q d tree,
    FoldTree 'True () I ColumnPrefix [DynQuerySql] tree
  ) => DynamicQuery rep q d where
    dynamicQuery =
      mconcat .
      intersperse " and " .
      fmap unDynQuerySql .
      foldTree @'True InitPrefix .
      dbValueQueryTree @rep @q @d
