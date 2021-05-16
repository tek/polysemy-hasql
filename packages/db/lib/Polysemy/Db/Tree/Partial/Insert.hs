module Polysemy.Db.Tree.Partial.Insert where

import GHC.TypeLits (ErrorMessage)
import Generics.SOP (All, hcmap)
import Type.Errors (TypeError)

import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldUpdate(FieldUpdate), PartialField, FieldPath (FieldName))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import qualified Polysemy.Db.Type.Data.Tree as Type

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField
type PartialCon = Type.Con () PartialField

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
