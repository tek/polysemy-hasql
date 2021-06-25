module Polysemy.Db.Tree.Merge where

import Generics.SOP (All, hcmap)

import Polysemy.Db.Data.FieldId (FieldId, NamedFields)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class MergeNodeAt (path :: [FieldId]) (patch :: Kind.Tree) (node :: Kind.Node) where
  mergeNodeAt :: Type.Tree t n patch -> Type.Node t n node -> Type.Node t n node

instance (
    path ~ (head : tail),
    All (MergeTreeAt path patch) trees
  ) => MergeNodeAt (head : tail) patch ('Kind.Prod d trees) where
  mergeNodeAt patch (Type.Prod n trees) =
    Type.Prod n (hcmap (Proxy @(MergeTreeAt path patch)) (mergeTreeAt @path patch) trees)

instance MergeNodeAt '[] ('Kind.Tree name eff patch) patch where
  mergeNodeAt (Type.Tree _ patch) _ =
    patch

instance MergeNodeAt path patch ('Kind.Prim d) where
  mergeNodeAt _ tree =
    tree

class MergeTreeAt (path :: [FieldId]) (patch :: Kind.Tree) (tree :: Kind.Tree) where
  mergeTreeAt :: Type.Tree t n patch -> Type.Tree t n tree -> Type.Tree t n tree

instance {-# overlappable #-} MergeTreeAt path patch tree where
  mergeTreeAt _ tree =
    tree

instance (
    MergeNodeAt path patch node
  ) => MergeTreeAt (field : path) patch ('Kind.Tree field eff node) where
  mergeTreeAt patch (Type.Tree t node) =
    Type.Tree t (mergeNodeAt @path patch node)

class MergeAt (path :: [FieldId]) (patch :: Kind.Tree) (tree :: Kind.Tree) where
  mergeAt :: Type.Tree t n patch -> Type.Tree t n tree -> Type.Tree t n tree

instance (
    MergeNodeAt (h : path) patch node
  ) => MergeAt (h : path) patch ('Kind.Tree name eff node) where
  mergeAt patch (Type.Tree t node) =
    Type.Tree t (mergeNodeAt @(h : path) patch node)

instance MergeAt '[] ('Kind.Tree name eff node) ('Kind.Tree name eff node) where
  mergeAt (Type.Tree t rnode) (Type.Tree _ _) =
    Type.Tree t rnode

type MergeAtNames names patch tree =
  MergeAt (NamedFields names) patch tree

mergeAtNames ::
  ∀ names patch tree t n .
  MergeAtNames names patch tree =>
  Type.Tree t n patch ->
  Type.Tree t n tree ->
  Type.Tree t n tree
mergeAtNames =
  mergeAt @(NamedFields names)

mergeAtName ::
  ∀ name patch tree t n .
  MergeAtNames '[name] patch tree =>
  Type.Tree t n patch ->
  Type.Tree t n tree ->
  Type.Tree t n tree
mergeAtName =
  mergeAtNames @'[name]

mergePayload ::
  ∀ patch tree t n .
  MergeAtNames '["_payload"] patch tree =>
  Type.Tree t n patch ->
  Type.Tree t n tree ->
  Type.Tree t n tree
mergePayload =
  mergeAtName @"_payload"
