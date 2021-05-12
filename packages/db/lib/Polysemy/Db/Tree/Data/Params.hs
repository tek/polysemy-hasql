module Polysemy.Db.Tree.Data.Params where

import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.TreeMeta (TM, TreeMeta, TreeMetaType)
import qualified Polysemy.Db.Type.Data.Tree as Type

data Params =
  Params {
    tag :: Type,
    treeParam :: Type,
    nodeParam :: Type -> Type,
    payload :: Type -> Type
  }

type family Tag (params :: Params) :: Type where
  Tag ('Params tag _ _ _) = tag

type family TreeParam (params :: Params) :: Type where
  TreeParam ('Params _ tree _ _) = tree

type family NodeParam (params :: Params) :: Type -> Type where
  NodeParam ('Params _ _ node _) = node

type family Payload (params :: Params) :: Type -> Type where
  Payload ('Params _ _ _ pf) = pf

type family PayloadT (params :: Params) (meta :: TreeMeta) :: Type where
  PayloadT p meta = Payload p (TreeMetaType meta)

type family PayloadM (params :: Params) :: TreeMeta -> Type where
  PayloadM params = TM (Payload params)

type family TTree (params :: Params) :: Kind.Tree -> Type where
  TTree ('Params _ t n _) = Type.Tree t n

type family TNode (params :: Params) :: Kind.Node -> Type where
  TNode ('Params _ t n _) = Type.Node t n
