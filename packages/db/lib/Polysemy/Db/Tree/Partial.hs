module Polysemy.Db.Tree.Partial (
  module Polysemy.Db.Tree.Partial,
  Partial (..),
  Partially (partially),
  PartialParams,
  PartialTree,
  PartialTag,
  PartialUpdate (..),
  UpdatePartialTree,
) where


import Polysemy.Db.Data.Partial (Partial (Partial), wrapPartial)
import Polysemy.Db.Data.PartialField (
  FieldPath (FieldName, FieldPath),
  FieldUpdate (FieldUpdate),
  PartialParams,
  PartialTag,
  PartialTree,
  Partially (partially),
  )
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data (GenDataTree (..), ReifyDataTree (..))
import Polysemy.Db.Tree.Partial.Insert (Insert (insert), PartialUpdate (..))
import Polysemy.Db.Tree.Partial.Update (UpdatePartialTree (..))

type family MkFieldPath (path :: k) :: FieldPath where
  MkFieldPath (p : ps) =
    'FieldPath (p : ps)
  MkFieldPath p =
    'FieldName p

field ::
  ∀ path (a :: Type) .
  a ->
  FieldUpdate (MkFieldPath path) a
field =
  FieldUpdate

(+>) ::
  ∀ (path :: FieldPath) (a :: Type) (tree :: Kind.Tree) .
  Insert path a tree =>
  PartialTree tree ->
  FieldUpdate path a ->
  PartialTree tree
(+>) =
  flip insert

(++>) ::
  ∀ (path :: FieldPath) (a :: Type) (d :: Type) (tree :: Kind.Tree) .
  Partially d tree =>
  Insert path a tree =>
  Partial d ->
  FieldUpdate path a ->
  Partial d
(++>) (Partial (tree :: PartialTree tree)) update =
  wrapPartial (insert update tree)

updatePartial ::
  ∀ (d :: Type) (dataTree :: Kind.Tree) (updateTree :: Kind.Tree) .
  GenDataTree d dataTree =>
  ReifyDataTree dataTree d =>
  UpdatePartialTree dataTree updateTree =>
  PartialTree updateTree ->
  d ->
  d
updatePartial updates d =
  reifyDataTree (updatePartialTree (genDataTree d) updates)
