module Polysemy.Db.Tree.Partial where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Foldable (Any)
import GHC.TypeLits (ErrorMessage)
import Generics.SOP (All, I(I), NP ((:*)), NS (Z, S), hcmap)
import Type.Errors (TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldPath (FieldPath, FieldName), FieldUpdate(FieldUpdate), PartialField)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Kind.Data.Tree (NodeDataType, TreeDataType)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.SOP.Error (Quoted, QuotedType)
import Polysemy.Db.Tree (ProdForSumTree, Root(root), Tree(tree))
import Polysemy.Db.Tree.Api (TreePrim(..))
import Polysemy.Db.Tree.Data (DataCon, DataParams, DataTree, GenDataTree (genDataTree), ReifyDataTree (reifyDataTree))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Data.TreeMeta (TM(TM), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import qualified Polysemy.Db.Type.Data.Tree as Type

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField
type PartialCon = Type.Con () PartialField
type PartialParams = 'Params PartialTag () PartialField

instance ProdForSumTree PartialTag 'True

instance TreePrim PartialTag PartialField name d where
  treePrim _ =
    PartialField.Keep

instance TreeEffects DefaultEffects rep d effs => TreeEffects PartialTag rep d effs where

partial ::
  ∀ d c .
  Root Auto PartialParams d c =>
  PartialTree c
partial =
  root @Auto @PartialParams @d PartialField.Keep

class InsertFieldCon (path :: FieldPath) (a :: *) (con :: Kind.Con) where
  insertFieldCon :: FieldUpdate path a -> PartialCon con -> PartialCon con

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldCon path a ('Kind.Con n ts) where
    insertFieldCon update (Type.Con sub) =
      Type.Con (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

class InsertFieldNode (path :: FieldPath) (a :: *) (n :: Kind.Node) where
  insertFieldNode :: FieldUpdate path a -> PartialNode n -> PartialNode n

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldNode path a ('Kind.Prod t ts) where
    insertFieldNode update (Type.Prod n sub) =
      Type.Prod n (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

instance (
    All (InsertFieldCon path a) cs
  ) => InsertFieldNode path a ('Kind.Sum t cs) where
    insertFieldNode update (Type.Sum n sub) =
      Type.Sum n (hcmap (Proxy @(InsertFieldCon path a)) (insertFieldCon update) sub)

instance (
    All (InsertFieldCon path a) cs
  ) => InsertFieldNode path a ('Kind.SumProd t cs) where
    insertFieldNode update (Type.SumProd n sub) =
      Type.SumProd n (hcmap (Proxy @(InsertFieldCon path a)) (insertFieldCon update) sub)

class InsertMatchingName (path :: FieldPath) (a :: *) (effs :: [*]) (node :: Kind.Node) where
  insertMatchingName :: FieldUpdate path a -> PartialNode node -> PartialNode node

instance (
    KnownSymbol name
  ) => InsertMatchingName ('FieldName name) d effs ('Kind.Prod d trees) where
    insertMatchingName (FieldUpdate d) (Type.Prod _ sub) =
      Type.Prod (PartialField.Update (symbolText @name) d) sub

instance (
    KnownSymbol name
  ) => InsertMatchingName ('FieldName name) d effs ('Kind.SumProd d trees) where
    insertMatchingName (FieldUpdate d) (Type.SumProd _ sub) =
      Type.SumProd (PartialField.Update (symbolText @name) d) sub

instance (
    KnownSymbol name
  ) => InsertMatchingName ('FieldName name) d effs ('Kind.Prim d) where
  insertMatchingName (FieldUpdate d) (Type.Prim _) =
    Type.Prim (PartialField.Update (symbolText @name) d)

class InsertNonmatchingName (path :: FieldPath) (a :: *) (effs :: [*]) (node :: Kind.Node) where
  insertNonmatchingName :: FieldUpdate path a -> PartialNode node -> PartialNode node

instance InsertNonmatchingName path a effs ('Kind.Prim d) where
  insertNonmatchingName _ =
    id

instance (
    All (InsertFieldTree path a) trees
  ) => InsertNonmatchingName path a effs ('Kind.Prod d trees) where
  insertNonmatchingName update (Type.Prod n trees) =
    Type.Prod n (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) trees)

instance (
    All (InsertFieldCon path a) trees
  ) => InsertNonmatchingName path a effs ('Kind.SumProd d trees) where
  insertNonmatchingName update (Type.SumProd n trees) =
    Type.SumProd n (hcmap (Proxy @(InsertFieldCon path a)) (insertFieldCon update) trees)

class InsertFieldTree (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertFieldTree :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    InsertMatchingName ('FieldName name) a effs node
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField name) effs node) where
  insertFieldTree update (Type.Tree t nd) =
    Type.Tree t (insertMatchingName @('FieldName name) @a @effs @node update nd)

instance {-# overlappable #-} (
    InsertNonmatchingName ('FieldName name) a effs node
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField _name) effs node) where
  insertFieldTree update (Type.Tree t nd) =
    Type.Tree t (insertNonmatchingName @('FieldName name) @a @effs @node update nd)

class InsertField (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertField :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    InsertFieldNode path a n
  ) => InsertField path a ('Kind.Tree ('NamedField _name) effs n) where
  insertField update (Type.Tree t n) =
    Type.Tree t (insertFieldNode @path @a @n update n)

class InsertFieldOrError (err :: Maybe ErrorMessage) (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertFieldOrError :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance TypeError err => InsertFieldOrError ('Just err) path a t where
  insertFieldOrError _ =
    id

instance InsertField path a t => InsertFieldOrError 'Nothing path a t where
  insertFieldOrError =
    insertField

type family MatchNodeType (a :: Type) (node :: Type) :: Bool where
  MatchNodeType a a =
    'True
  MatchNodeType _ _ =
    'False

type family InsertableConName (a :: Type) (name :: Symbol) (con :: Kind.Con) :: Bool where
  InsertableConName a name ('Kind.Con _ sub) =
    Any (InsertableNameExp a name) @@ sub

data InsertableConNameExp (a :: Type) (name :: Symbol) :: Kind.Con -> Exp Bool
type instance Eval (InsertableConNameExp a name con) =
  InsertableConName a name con

type family InsertableName (a :: Type) (name :: Symbol) (tree :: Kind.Tree) :: Bool where
  InsertableName a name ('Kind.Tree ('NamedField name) _ node) =
    MatchNodeType a (NodeDataType node)
  InsertableName a name ('Kind.Tree _ _ ('Kind.Prod _ sub)) =
    Any (InsertableNameExp a name) @@ sub
  InsertableName a name ('Kind.Tree _ _ ('Kind.Sum _ sub)) =
    Any (InsertableConNameExp a name) @@ sub
  InsertableName a name ('Kind.Tree _ _ ('Kind.SumProd _ sub)) =
    Any (InsertableConNameExp a name) @@ sub
  InsertableName _ _ ('Kind.Tree _ _ ('Kind.Prim _)) =
    'False

data InsertableNameExp (a :: Type) (name :: Symbol) :: Kind.Tree -> Exp Bool
type instance Eval (InsertableNameExp a name tree) =
  InsertableName a name tree

type family InsertableResult (d :: Type) (a :: Type) (path :: FieldPath) (found :: Bool) :: Maybe ErrorMessage where
  InsertableResult _ _ _ 'True =
    'Nothing
  InsertableResult d a ('FieldName name) 'False =
    'Just (
      "Could not find a field of type " <> QuotedType a <> " named " <> Quoted name <> " in the type " <> QuotedType d %
      "While attempting to construct a partial update"
      )

-- TODO also check field type
type family Insertable (a :: Type) (path :: FieldPath) (tree :: Kind.Tree) :: Maybe ErrorMessage where
  Insertable a ('FieldName name) tree =
    InsertableResult (TreeDataType tree) a ('FieldName name) (InsertableName a name tree)

(+>) ::
  ∀ path a t .
  InsertFieldOrError (Insertable a path t) path a t =>
  PartialTree t ->
  FieldUpdate path a ->
  PartialTree t
(+>) =
  flip (insertFieldOrError @(Insertable a path t))

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

class UpdatePartialSum (ds :: [Kind.Con]) (us :: [Kind.Con]) where
  updatePartialSum :: NS DataCon ds -> NS PartialCon us -> NS DataCon ds

class UpdatePartialSumProd (ds :: [Kind.Con]) (us :: [Kind.Con]) where
  updatePartialSumProd :: NS DataCon ds -> NP PartialCon us -> NS DataCon ds

instance UpdatePartialSumProd '[] us where
  updatePartialSumProd =
    const

instance (
    UpdatePartialProd d u,
    UpdatePartialSumProd ds us
  ) => UpdatePartialSumProd ('Kind.Con _n d : ds) ('Kind.Con _n u : us) where
  updatePartialSumProd (Z (Type.Con d)) ((Type.Con u) :* _) =
    Z (Type.Con (updatePartialProd d u))
  updatePartialSumProd (S d) (_ :* us) =
    S (updatePartialSumProd d us)

class UpdatePartialTree (dataTree :: Kind.Tree) (updateTree :: Kind.Tree) where
  updatePartialTree :: DataTree dataTree -> PartialTree updateTree -> DataTree dataTree

updateNode :: I d -> PartialField d -> I d
updateNode old = \case
  PartialField.Update _ new ->
    pure new
  PartialField.Keep ->
    old

instance UpdatePartialTree ('Kind.Tree name eff ('Kind.Prim d)) ('Kind.Tree name eff ('Kind.Prim d)) where
  updatePartialTree (Type.Tree t (Type.Prim old)) (Type.Tree _ (Type.Prim new)) =
    Type.Tree t (Type.Prim (updateNode old new))

instance (
    UpdatePartialSum subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.Sum d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Sum old subData)) (Type.Tree () (Type.Sum new subUpdate)) =
    Type.Tree () (Type.Sum (updateNode old new) (updatePartialSum subData subUpdate))

instance (
    UpdatePartialProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Prod d subData)) ('Kind.Tree name eff ('Kind.Prod d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Prod old subData)) (Type.Tree () (Type.Prod new subUpdate)) =
    Type.Tree () (Type.Prod (updateNode old new) (updatePartialProd subData subUpdate))

instance (
    meta ~ 'TreeMeta name Auto d,
    Tree DataParams meta ('Kind.Tree name eff ('Kind.Sum d subData)),
    UpdatePartialSumProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.SumProd d subUpdate)) where
    updatePartialTree (Type.Tree () (Type.Sum old subData)) (Type.Tree () (Type.SumProd PartialField.Keep subUpdate)) =
      Type.Tree () (Type.Sum old (updatePartialSumProd subData subUpdate))
    updatePartialTree (Type.Tree () (Type.Sum _ _)) (Type.Tree () (Type.SumProd (PartialField.Update _ new) _)) =
      tree @DataParams @meta (TM (I new))

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
