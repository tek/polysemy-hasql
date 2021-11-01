module Polysemy.Hasql.Table.Query.Update where

import Hasql.DynamicStatements.Snippet (Snippet, encoderAndParam, sql)
import Polysemy.Db.Data.Partial (PartialFor)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Data.Rep (Prim)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Effect (UnwrapEffect)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreePrim (..), foldTree)
import Polysemy.Db.Tree.Partial (PartialTree)

import Polysemy.Hasql.Data.DbType (Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import Polysemy.Hasql.Data.Table (Table, selector, structure)
import Polysemy.Hasql.Data.Where (Where (Where))
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN (queryValueNoN))
import Polysemy.Hasql.Tree.Table (PrimColumn)

newtype PartialSql =
  PartialSql { unPartialSql :: Snippet }
  deriving newtype (Semigroup, Monoid)

type BuildPartialSql d tree u =
  (
    PartialFor d tree u,
    FoldTree 'True () PartialField [PartialSql] tree
  )

commaSeparatedSnippet ::
  [Snippet] ->
  Snippet
commaSeparatedSnippet =
  mconcat . intersperse ", "

class ForcePrim d eff prim eff' | d eff prim -> eff'

instance ForcePrim d eff 'True Prim

instance ForcePrim d eff 'False eff

class AdaptPartialEffect eff d eff' | eff d -> eff'

instance (
    PrimColumn d prim,
    ForcePrim d eff prim eff'
  ) => AdaptPartialEffect eff d eff'

class AdaptPartialEffects (effs :: [Type]) (d :: Type) (effs' :: [Type]) | effs d -> effs'

instance AdaptPartialEffects '[] d '[]

instance (
    UnwrapEffect eff d d',
    AdaptPartialEffect eff d eff',
    AdaptPartialEffects effs d' effs'
  ) => AdaptPartialEffects (eff : effs) d (eff' : effs')

instance (
    AdaptPartialEffects effs d effs',
    QueryValueNoN effs' d
  ) => FoldTreePrim root () PartialField [PartialSql] name effs d where
    foldTreePrim = \case
      PartialField.Keep -> mempty
      PartialField.Update name value ->
        [PartialSql (sql (encodeUtf8 (quotedDbId name)) <> " = " <> encoderAndParam (queryValueNoN @effs' @d) value)]

update ::
  FoldTree 'True () PartialField [PartialSql] tree =>
  Table d ->
  Maybe (Where query d) ->
  query ->
  PartialTree tree ->
  Snippet
update table qWhere q tree =
  sql [exon|update #{sel} set |] <>
  commaSeparatedSnippet (unPartialSql <$> foldTree @'True tree) <>
  foldMap whereSnippet qWhere <>
  " returning " <>
  sql (encodeUtf8 cols)
  where
    Selector (SqlCode (encodeUtf8 -> sel)) =
      table ^. selector
    whereSnippet (Where _ qw) =
      " where " <> qw q
    SqlCode cols =
      selectColumns (table ^. structure)
