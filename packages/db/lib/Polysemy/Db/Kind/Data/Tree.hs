module Polysemy.Db.Kind.Data.Tree where

import Polysemy.Db.Data.FieldId (FieldId)

data Node a =
  Prim {
    tpe :: *
  }
  |
  Prod {
    tpe :: *,
    sub :: [Tree a]
  }
  |
  Sum {
    tpe :: *,
    sub :: [Tree a]
  }

type family ColumnDataType (dt :: Node a) :: * where
  ColumnDataType ('Prim d) = d
  ColumnDataType ('Prod d _) = d
  ColumnDataType ('Sum d _) = d

data Tree a =
  Tree {
    name :: FieldId,
    eff :: a,
    node :: Node a
  }
