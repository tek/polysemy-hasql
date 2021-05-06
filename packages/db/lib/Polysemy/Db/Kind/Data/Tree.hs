module Polysemy.Db.Kind.Data.Tree where

import Polysemy.Db.Data.FieldId (FieldId)

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
    sub :: [Tree]
  }
  |
  SumProd {
    tpe :: *,
    sub :: [Tree]
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

type family TreeDataType (tree :: Tree) :: * where
  TreeDataType ('Tree _ _ node) = NodeDataType node
