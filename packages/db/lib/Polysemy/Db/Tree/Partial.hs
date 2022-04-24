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


import Polysemy.Db.Data.Partial (Partial (Partial), PartialFor, wrapPartial)
import Polysemy.Db.Data.PartialField (
  FieldPath (FieldName, FieldPath),
  FieldUpdate (FieldUpdate),
  PartialParams,
  PartialTag,
  PartialTree,
  Partially (partially),
  )
import Polysemy.Db.Data.PartialUpdate (PartialUpdate (PartialUpdate))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data (GenDataTree (..), ReifyDataTree (..))
import Polysemy.Db.Tree.Partial.Insert (Insert (insert))
import Polysemy.Db.Tree.Partial.Update (UpdatePartialTree (..))

type MkFieldPath :: ∀ k . k -> FieldPath
type family MkFieldPath path :: FieldPath where
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

(++>) ::
  ∀ (path :: FieldPath) (a :: Type) (tree :: Kind.Tree) .
  Insert path a tree =>
  PartialTree tree ->
  FieldUpdate path a ->
  PartialTree tree
(++>) =
  flip insert

(+>) ::
  ∀ (path :: FieldPath) (a :: Type) (d :: Type) (tree :: Kind.Tree) (dataTree :: Kind.Tree) .
  PartialFor d tree dataTree =>
  Insert path a tree =>
  Partial d ->
  FieldUpdate path a ->
  Partial d
(+>) (Partial (tree :: PartialTree tree)) update =
  wrapPartial (insert update tree)

patch ::
  ∀ (d :: Type) (dataTree :: Kind.Tree) (tree :: Kind.Tree) .
  PartialFor d tree dataTree =>
  Partial d ->
  d ->
  d
patch (Partial updates) d =
  reifyDataTree (updatePartialTree (genDataTree d) updates)

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
