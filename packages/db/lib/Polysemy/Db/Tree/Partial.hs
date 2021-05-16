module Polysemy.Db.Tree.Partial where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Foldable (Any)
import GHC.TypeLits (ErrorMessage)
import Generics.SOP (I(I), NP ((:*)), NS (Z, S))
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldPath (FieldPath, FieldName), FieldUpdate(FieldUpdate), PartialField)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Kind.Data.Tree (NodeDataType, TreeDataType)
import Polysemy.Db.SOP.Error (Quoted, QuotedType)
import Polysemy.Db.Tree (ProdForSumTree, Root(root), Tree(tree))
import Polysemy.Db.Tree.Api (TreePrim(..))
import Polysemy.Db.Tree.Data (DataCon, DataParams, DataTree, GenDataTree (genDataTree), ReifyDataTree (reifyDataTree))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Data.TreeMeta (TM(TM), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.Fold (FoldTree)
import Polysemy.Db.Tree.Partial.Insert (InsertFieldOrError(..))
import qualified Polysemy.Db.Type.Data.Tree as Type

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField
type PartialCon = Type.Con () PartialField
type PartialParams = 'Params PartialTag () PartialField
type FoldPartialTree = FoldTree () PartialField

instance ProdForSumTree PartialTag 'True

instance TreePrim PartialTag PartialField name d where
  treePrim _ =
    PartialField.Keep

instance TreeEffects DefaultEffects rep d effs => TreeEffects PartialTag rep d effs where

data PTree (d :: Type) where
  PTree :: PartialTree tree -> PTree d

class Partial d tree | d -> tree where
  partial :: PartialTree tree

instance (
    Root Auto PartialParams d tree
  ) => Partial d tree where
  partial =
    root @Auto @PartialParams @d PartialField.Keep

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

class Insert (path :: FieldPath) (a :: Type) (tree :: Kind.Tree) where
  insert :: FieldUpdate path a -> PartialTree tree -> PartialTree tree

instance (
    insertable ~ Insertable a path tree,
    InsertFieldOrError insertable path a tree
  ) => Insert path a tree where
  insert =
    insertFieldOrError @insertable

type InsertName (name :: Symbol) (a :: Type) (tree :: Kind.Tree) = Insert ('FieldName name) a tree

type family InsertAllProd (root :: Kind.Tree) (name :: Symbol) (prefix :: [Symbol]) (trees :: [Kind.Tree]) :: Constraint where
  InsertAllProd root name prefix '[tree] = InsertAllPre root (name : prefix) tree
  InsertAllProd root name prefix (tree : trees) =
    (InsertAllPre root (name : prefix) tree, InsertAllProd root name prefix trees)

type family InsertAllNode (root :: Kind.Tree) (name :: Symbol) (prefix :: [Symbol]) (node :: Kind.Node) :: Constraint where
  InsertAllNode root name _ ('Kind.Prim d) =
    -- (Insert ('FieldPath (name : prefix)) d root, Insert ('FieldName name) d root)
    (Insert ('FieldName name) d root)
  InsertAllNode root name prefix ('Kind.Prod _ node) =
    InsertAllProd root name prefix node

type family InsertAllPre (root :: Kind.Tree) (prefix :: [Symbol]) (tree :: Kind.Tree) :: Constraint where
  InsertAllPre root prefix ('Kind.Tree ('NamedField name) _ node) =
    InsertAllNode root name prefix node

type family InsertAll (tree :: Kind.Tree) :: Constraint where
  InsertAll ('Kind.Tree name effs ('Kind.Prod d node)) =
    InsertAllNode ('Kind.Tree name effs ('Kind.Prod d node)) "" '[] ('Kind.Prod d node)

(+>) ::
  ∀ (path :: FieldPath) (a :: Type) (tree :: Kind.Tree) .
  Insert path a tree =>
  PartialTree tree ->
  FieldUpdate path a ->
  PartialTree tree
(+>) =
  flip insert

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
