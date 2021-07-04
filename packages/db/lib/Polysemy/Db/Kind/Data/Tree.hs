module Polysemy.Db.Kind.Data.Tree where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import qualified Polysemy.Db.Data.Rep as Rep
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.SOP.Constraint (DataNameF)
import Polysemy.Db.Tree.Data.Effect (Adt)
import Polysemy.Db.Tree.Meta (AdtMeta')

data Con =
  Con {
    num :: Nat,
    name :: FieldId,
    sub :: [Tree]
  }
  |
  ConUna {
    num :: Nat,
    name :: FieldId,
    tree :: Tree
  }

-- TODO SumProd should not be necessary anymore, we can now offload the distinction to the implementation, i.e.
-- Tree/Table
data Node =
  Prim {
    tpe :: Type
  }
  |
  Prod {
    tpe :: Type,
    sub :: [Tree]
  }
  |
  Sum {
    tpe :: Type,
    cons :: [Con]
  }
  |
  SumProd {
    tpe :: Type,
    cons :: [Con]
  }

data Tree =
  Tree {
    name :: FieldId,
    eff :: [Type],
    node :: Node
  }

type family NodeDataType (node :: Node) :: Type where
  NodeDataType ('Prim d) = d
  NodeDataType ('Prod d _) = d
  NodeDataType ('Sum d _) = d
  NodeDataType ('SumProd d _) = d

type family TreeDataType (tree :: Tree) :: Type where
  TreeDataType ('Tree _ _ node) = NodeDataType node

type family AdtTree (field :: Symbol) (d :: Type) (node :: Node) :: Tree where
  AdtTree field d node =
    'Tree ('NamedField field) '[Adt (AdtMeta' Auto d) Auto] node

type family AdtRoot (d :: Type) (node :: Node) :: Tree where
  AdtRoot d node =
    AdtTree (DataNameF d) d node

type family ProdTree (field :: Symbol) (d :: Type) (trees :: [Tree]) :: Tree where
  ProdTree field d trees =
    AdtTree field d ('Prod d trees)

type family ProdRoot (d :: Type) (trees :: [Tree]) :: Tree where
  ProdRoot d node =
    ProdTree (DataNameF d) d node

type family PrimTreeWith (field :: Symbol) (rep :: [Type]) (d :: Type) :: Tree where
  PrimTreeWith field rep d =
    'Tree ('NamedField field) rep ('Prim d)

type family PrimTree (field :: Symbol) (d :: Type) :: Tree where
  PrimTree field d =
    PrimTreeWith field '[Rep.Prim] d

type family UidTree (field :: Symbol) (i :: Type) (d :: Type) (rep :: Type) (node :: Node) :: Tree where
  UidTree field i d rep node =
    ProdTree field (Uid i d) '[
      PrimTree "_id" i,
      'Tree ('NamedField "_payload") '[Adt (AdtMeta' rep d) rep] node
    ]

type family UidRoot (i :: Type) (d :: Type) (rep :: Type) (node :: Node) :: Tree where
  UidRoot i d rep node =
    UidTree (DataNameF d) i d rep node
