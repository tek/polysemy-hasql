module Polysemy.Db.Tree.Partial (
  module Polysemy.Db.Tree.Partial,
  Partial(partial),
  PartialParams,
  PartialTree,
  PartialTag,
  UpdatePartialTree,
) where

import Polysemy.Db.Data.PartialField (
  FieldPath (FieldPath, FieldName),
  FieldUpdate(FieldUpdate),
  Partial(partial),
  PartialParams,
  PartialTag,
  PartialTree,
  )
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data (GenDataTree(..), ReifyDataTree(..))
import Polysemy.Db.Tree.Partial.Insert (Insert(..))
import Polysemy.Db.Tree.Partial.Update (UpdatePartialTree(..))

type family MkFieldPath (path :: k) :: FieldPath where
  MkFieldPath (p : ps) =
    'FieldPath (p : ps)
  MkFieldPath p =
    'FieldName p

field ::
  ∀ path (a :: *) .
  a ->
  FieldUpdate (MkFieldPath path) a
field =
  FieldUpdate

type InsertName (name :: Symbol) (a :: Type) (tree :: Kind.Tree) =
  Insert ('FieldName name) a tree

(+>) ::
  ∀ (path :: FieldPath) (a :: Type) (tree :: Kind.Tree) .
  Insert path a tree =>
  PartialTree tree ->
  FieldUpdate path a ->
  PartialTree tree
(+>) =
  flip insert

updatePartial ::
  ∀ d (dataTree :: Kind.Tree) (updateTree :: Kind.Tree) .
  GenDataTree d dataTree =>
  ReifyDataTree dataTree d =>
  UpdatePartialTree dataTree updateTree =>
  PartialTree updateTree ->
  d ->
  d
updatePartial updates d =
  reifyDataTree (updatePartialTree (genDataTree d) updates)
