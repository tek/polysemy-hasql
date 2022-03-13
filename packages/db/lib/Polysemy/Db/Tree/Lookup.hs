module Polysemy.Db.Tree.Lookup where

import Generics.SOP (NP, hd, tl)

import Polysemy.Db.Data.FieldId (FieldId (NamedField), NamedFields)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Kind.Data.Tree (NodeDataType)
import Polysemy.Db.SOP.Constraint (DataNameF)
import qualified Polysemy.Db.Type.Data.Tree as Type

class LookupProd (path :: [FieldId]) (trees :: [Kind.Tree]) (sub :: Kind.Tree) | path trees -> sub where
  lookupProd :: NP (Type.Tree t n) trees -> Type.Tree t n sub

instance {-# overlappable #-} (
    LookupProd path trees sub
  ) => LookupProd path (tree : trees) sub where
  lookupProd =
    lookupProd @path @trees . tl

instance (
    LookupTree path ('Kind.Tree field eff node) sub
  ) => LookupProd (field : path) ('Kind.Tree field eff node : trees) sub where
  lookupProd =
    lookupTree @path . hd

class LookupNode (path :: [FieldId]) (node :: Kind.Node) (sub :: Kind.Tree) | path node -> sub where
  lookupNode :: Type.Node t n node -> Type.Tree t n sub

instance (
    LookupProd path trees sub
  ) => LookupNode path ('Kind.Prod d trees) sub where
  lookupNode (Type.Prod _ trees) =
    lookupProd @path trees

class LookupTree (path :: [FieldId]) (tree :: Kind.Tree) (sub :: Kind.Tree) | path tree -> sub where
  lookupTree :: Type.Tree t n tree -> Type.Tree t n sub

instance (
    name ~ 'NamedField (DataNameF (NodeDataType node))
  ) => LookupTree '[] ('Kind.Tree field eff node) ('Kind.Tree name eff node) where
  lookupTree (Type.Tree t node) =
    Type.Tree t node

instance (
    path ~ (h : t),
    LookupNode path node sub
  ) => LookupTree (h : t) ('Kind.Tree field eff node) sub where
    lookupTree (Type.Tree _ node) =
      lookupNode @path node

class Lookup (path :: [FieldId]) (tree :: Kind.Tree) (sub :: Kind.Tree) | path tree -> sub where
  lookup :: Type.Tree t n tree -> Type.Tree t n sub

instance (
    LookupNode path node sub
  ) => Lookup path ('Kind.Tree name eff node) sub where
    lookup (Type.Tree _ node) =
      lookupNode @path node

type LookupNames names tree sub =
  Lookup (NamedFields names) tree sub

lookupNames ::
  ∀ names tree sub t n .
  LookupNames names tree sub =>
  Type.Tree t n tree ->
  Type.Tree t n sub
lookupNames =
  lookup @(NamedFields names)
