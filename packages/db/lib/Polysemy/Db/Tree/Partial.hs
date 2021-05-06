module Polysemy.Db.Tree.Partial where

import Generics.SOP (All, NP ((:*)), NS (Z, S), hcmap)

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldPath (FieldName), FieldUpdate(FieldUpdate), PartialField)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.Tree (Params(Params), Root, SumIndexTree, SumOrProd, TreeConPayload(..), TreeCons(..), TreePrim(..), root)
import Polysemy.Db.Tree.Data (DataTree, GenDataTree (genDataTree), ReifyDataTree (reifyDataTree))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import qualified Polysemy.Db.Type.Data.Tree as Type

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField

instance SumOrProd PartialTag 'False

instance TreePrim PartialTag PartialField a name d where
  treePrim _ =
    PartialField.Keep

instance TreeEffects DefaultEffects rep d effs => TreeEffects PartialTag rep d effs where

instance TreeCons PartialTag () () where
  treeCons _ =
    ()

instance TreeConPayload PartialTag name () where
  treeConPayload =
    ()

partial ::
  ∀ d c .
  Root Auto ('Params PartialTag () PartialField) d () c =>
  PartialTree c
partial =
  root @Auto @('Params PartialTag () PartialField) @d ()

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
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField name) effs ('Kind.Prim a)) where
  insertFieldTree (FieldUpdate a) (Type.Tree t _) =
    Type.Tree t (Type.Prim (PartialField.Update (symbolText @name) a))

instance {-# overlappable #-} InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField _name) effs ('Kind.Prim _d)) where
  insertFieldTree _ =
    id

instance (
    All (InsertFieldTree ('FieldName name) a) trees
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField _name) effs ('Kind.Prod d trees)) where
  insertFieldTree update (Type.Tree t (Type.Prod trees)) =
    Type.Tree t (Type.Prod (hcmap (Proxy @(InsertFieldTree ('FieldName name) a)) (insertFieldTree update) trees))

class InsertField (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertField :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    InsertFieldNode path a n
  ) => InsertField path a ('Kind.Tree ('NamedField _name) effs n) where
  insertField update (Type.Tree t n) =
    Type.Tree t (insertFieldNode @path @a @n update n)

(+>) ::
  InsertField path a t =>
  PartialTree t ->
  FieldUpdate path a ->
  PartialTree t
(+>) =
  flip insertField

class UpdatePartialProd (ds :: [Kind.Tree]) (us :: [Kind.Tree]) where
  updatePartialProd :: NP DataTree ds -> NP PartialTree us -> NP DataTree ds

instance UpdatePartialProd '[] '[] where
  updatePartialProd =
    const

instance (
    UpdatePartialTree d u,
    UpdatePartialProd ds us
  ) => UpdatePartialProd (d : ds) (u : us) where
    updatePartialProd (d :* ds) (u :* us) =
      updatePartialTree d u :* updatePartialProd ds us

class UpdatePartialSum (ds :: [Kind.Tree]) (us :: [Kind.Tree]) where
  updatePartialSum :: NS DataTree ds -> NS PartialTree us -> NS DataTree ds

class UpdatePartialSumProd (ds :: [Kind.Tree]) (us :: [Kind.Tree]) where
  updatePartialSumProd :: NS DataTree ds -> NP PartialTree us -> NS DataTree ds

instance UpdatePartialSumProd '[] us where
  updatePartialSumProd =
    const

instance (
    UpdatePartialTree d u,
    UpdatePartialSumProd ds us
  ) => UpdatePartialSumProd (d : ds) (u : us) where
  updatePartialSumProd (Z d) (u :* _) =
    Z (updatePartialTree d u)
  updatePartialSumProd (S d) (_ :* us) =
    S (updatePartialSumProd d us)

class UpdatePartialTree (dataTree :: Kind.Tree) (updateTree :: Kind.Tree) where
  updatePartialTree :: DataTree dataTree -> PartialTree updateTree -> DataTree dataTree

instance UpdatePartialTree ('Kind.Tree name eff ('Kind.Prim d)) ('Kind.Tree name eff ('Kind.Prim d)) where
  updatePartialTree (Type.Tree t (Type.Prim old)) = \case
    Type.Tree _ (Type.Prim (PartialField.Update _ new)) ->
      Type.Tree t (Type.Prim (Identity new))
    Type.Tree _ (Type.Prim PartialField.Keep) ->
      Type.Tree t (Type.Prim old)

-- instance (
--     All UpdatePartialTree sub
--   ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d sub)) where
--   updatePartialTree (Type.Tree () (Type.Sum subData)) (Type.Tree () (Type.Sum subUpdate)) =
--     Type.Tree () (Type.Sum (hczipWith (Proxy @UpdatePartialTree) updatePartialTree subData subUpdate))

instance (
    UpdatePartialSum subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.Sum d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Sum subData)) (Type.Tree () (Type.Sum subUpdate)) =
    Type.Tree () (Type.Sum (updatePartialSum subData subUpdate))

instance (
    UpdatePartialProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Prod d subData)) ('Kind.Tree name eff ('Kind.Prod d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Prod subData)) (Type.Tree () (Type.Prod subUpdate)) =
    Type.Tree () (Type.Prod (updatePartialProd subData subUpdate))

instance (
    UpdatePartialSumProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.Prod d (SumIndexTree : subUpdate))) where
  updatePartialTree (Type.Tree () (Type.Sum subData)) (Type.Tree () (Type.Prod ( _ :* subUpdate))) =
    Type.Tree () (Type.Sum (updatePartialSumProd subData subUpdate))

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
