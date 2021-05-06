module Polysemy.Db.Tree.Hoist where

import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type
import Generics.SOP (hcmap, All)

class HoistNode (f :: Type -> Type) (g :: Type -> Type) (node :: Kind.Node) where
  hoistNode :: (∀ a . f a -> g a) -> Type.Node t f node -> Type.Node t g node

instance HoistNode f g ('Kind.Prim d) where
  hoistNode f (Type.Prim fd) =
    Type.Prim (f fd)

instance (
    All (HoistNodeTree f g) trees
  ) => HoistNode f g ('Kind.Prod d trees) where
  hoistNode f (Type.Prod n trees) =
    Type.Prod (f n) (hcmap (Proxy @(HoistNodeTree f g)) (hoistNodeTree f) trees)

instance (
    All (HoistNodeTree f g) trees
  ) => HoistNode f g ('Kind.Sum d trees) where
  hoistNode f (Type.Sum n trees) =
    Type.Sum (f n) (hcmap (Proxy @(HoistNodeTree f g)) (hoistNodeTree f) trees)

class HoistNodeTree (f :: Type -> Type) (g :: Type -> Type) (tree :: Kind.Tree) where
  hoistNodeTree :: (∀ a . f a -> g a) -> Type.Tree t f tree -> Type.Tree t g tree

instance (
    HoistNode f g node
  ) => HoistNodeTree f g ('Kind.Tree name eff node) where
  hoistNodeTree f (Type.Tree t node) =
    Type.Tree t (hoistNode f node)
