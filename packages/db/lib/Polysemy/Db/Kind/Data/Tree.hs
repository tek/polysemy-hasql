module Polysemy.Db.Kind.Data.Tree where

import Polysemy.Db.Data.FieldId (FieldId)

-- TODO cons have to be numbered, so the Where machinery can match them, since Rep cons may have different names
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

data Node =
  Prim {
    tpe :: *
  }
  |
  Prod {
    tpe :: *,
    sub :: [Tree]
  }
  |
  Sum {
    tpe :: *,
    cons :: [Con]
  }
  |
  SumProd {
    tpe :: *,
    cons :: [Con]
  }


data Tree =
  Tree {
    name :: FieldId,
    eff :: [*],
    node :: Node
  }

type family NodeDataType (node :: Node) :: * where
  NodeDataType ('Prim d) = d
  NodeDataType ('Prod d _) = d
  NodeDataType ('Sum d _) = d
  NodeDataType ('SumProd d _) = d

type family TreeDataType (tree :: Tree) :: * where
  TreeDataType ('Tree _ _ node) = NodeDataType node
