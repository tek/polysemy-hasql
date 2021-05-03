module Polysemy.Db.Tree.Partial where

import Generics.SOP (All, hcmap, hczipWith)

import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldPath (FieldName), FieldUpdate(FieldUpdate), PartialField)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.Tree (
  Params(Params),
  Root,
  TreePayload(..),
  TreePrim(..),
  TreeProduct(..),
  TreeProductElem (treeProductElem),
  root,
  )
import Polysemy.Db.Tree.Data (DataTree, GenDataTree (genDataTree), ReifyDataTree (reifyDataTree))
import Polysemy.Db.Tree.Meta (TreeMeta(TreeMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField

instance TreePrim PartialTag PartialField a name d where
  treePrim _ =
    PartialField.Keep

instance TreePayload PartialTag () d meta where
  treePayload _ =
    ()

instance TreeProduct PartialTag d () where
  treeProduct _ =
    ()

instance TreeProductElem PartialTag () () ('TreeMeta name rep d) () where
  treeProductElem =
    undefined

partialTree' ::
  ∀ d c .
  Root ('Params PartialTag () PartialField) d () c =>
  PartialTree c
partialTree' =
  root @('Params PartialTag () PartialField) @d ()

treePure :: a -> Type.Tree t n sub -> Type.Tree t n sub
treePure =
  undefined

class InsertFieldNode (path :: FieldPath) (a :: *) (n :: Kind.Node) where
  insertFieldNode :: FieldUpdate path a -> PartialNode n -> PartialNode n

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldNode path a ('Kind.Prod t ts) where
    insertFieldNode update (Type.Prod sub) =
      Type.Prod (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldNode path a ('Kind.Sum t ts) where
    insertFieldNode update (Type.Sum sub) =
      Type.Sum (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

class InsertFieldTree (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertFieldTree :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    KnownSymbol name
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField name) '[] ('Kind.Prim a)) where
  insertFieldTree (FieldUpdate a) (Type.Tree t _) =
    Type.Tree t (Type.Prim (PartialField.Update (symbolText @name) a))

instance {-# overlappable #-} InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField _name) '[] _n) where
  insertFieldTree _ =
    id

class InsertField (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertField :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    InsertFieldNode path a n
  ) => InsertField path a ('Kind.Tree ('NamedField _name) '[] n) where
  insertField update (Type.Tree t n) =
    Type.Tree t (insertFieldNode @path @a @n update n)

(...>) ::
  InsertField path a t =>
  PartialTree t ->
  FieldUpdate path a ->
  PartialTree t
(...>) =
  flip insertField

class UpdatePartialTree (t :: Kind.Tree) where
  updatePartialTree :: DataTree t -> PartialTree t -> DataTree t

instance UpdatePartialTree ('Kind.Tree name eff ('Kind.Prim d)) where
  updatePartialTree (Type.Tree t (Type.Prim old)) = \case
    Type.Tree _ (Type.Prim (PartialField.Update _ new)) ->
      Type.Tree t (Type.Prim (Identity new))
    Type.Tree _ (Type.Prim PartialField.Keep) ->
      Type.Tree t (Type.Prim old)

instance (
    All UpdatePartialTree sub
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Prod d sub)) where
  updatePartialTree (Type.Tree () (Type.Prod subData)) (Type.Tree () (Type.Prod subUpdate)) =
    Type.Tree () (Type.Prod (hczipWith (Proxy @UpdatePartialTree) updatePartialTree subData subUpdate))

updatePartialTree' ::
  ∀ d (tree :: Kind.Tree) .
  GenDataTree d tree =>
  ReifyDataTree tree d =>
  UpdatePartialTree tree =>
  PartialTree tree ->
  d ->
  d
updatePartialTree' updates d =
  reifyDataTree (updatePartialTree (genDataTree d) updates)
