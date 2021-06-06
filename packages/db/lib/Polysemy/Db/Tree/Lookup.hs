module Polysemy.Db.Tree.Lookup where

import Fcf (Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Prelude hiding (lookup)

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type
import Generics.SOP (NP, tl, hd)
import Polysemy.Db.SOP.Error (ErrorWithType)
import Polysemy.Db.SOP.Constraint (DataNameF)
import Polysemy.Db.Kind.Data.Tree (NodeDataType)

class LookupProd (path :: [FieldId]) (trees :: [Kind.Tree]) (sub :: Kind.Tree) where
  lookupProd :: NP (Type.Tree t n) trees -> Type.Tree t n sub

instance (
    ErrorWithType "lookup: invalid path" path
  ) => LookupProd path '[] sub where
    lookupProd =
      error "impossible"

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

class LookupNode (path :: [FieldId]) (node :: Kind.Node) (sub :: Kind.Tree) where
  lookupNode :: Type.Node t n node -> Type.Tree t n sub

instance (
    LookupProd path trees sub
  ) => LookupNode path ('Kind.Prod d trees) sub where
  lookupNode (Type.Prod _ trees) =
    lookupProd @path trees

class LookupTree (path :: [FieldId]) (tree :: Kind.Tree) (sub :: Kind.Tree) where
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

class Lookup (path :: [FieldId]) (tree :: Kind.Tree) (sub :: Kind.Tree) where
  lookup :: Type.Tree t n tree -> Type.Tree t n sub

instance (
    LookupNode path node sub
  ) => Lookup path ('Kind.Tree name eff node) sub where
    lookup (Type.Tree _ node) =
      lookupNode @path node

lookupNames ::
  ∀ names tree sub t n .
  Lookup (FMap (Pure1 'NamedField) @@ names) tree sub =>
  Type.Tree t n tree ->
  Type.Tree t n sub
lookupNames =
  lookup @(FMap (Pure1 'NamedField) @@ names)
