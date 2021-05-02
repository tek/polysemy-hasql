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

data Tree =
  Tree {
    name :: FieldId,
    eff :: [*],
    node :: Node
  }

type family ColumnDataType (dt :: Node) :: * where
  ColumnDataType ('Prim d) = d
  ColumnDataType ('Prod d _) = d
  ColumnDataType ('Sum d _) = d
