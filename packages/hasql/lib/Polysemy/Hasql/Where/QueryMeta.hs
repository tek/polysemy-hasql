module Polysemy.Hasql.Where.QueryMeta where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Foldable (FoldMap)
import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind

data QueryMeta =
  QueryMeta {
    rep :: Kind.Tree,
    query :: FieldId,
    fields :: [FieldId]
  }

data ConFieldNames :: Kind.Con -> Exp [FieldId]

type instance Eval (ConFieldNames ('Kind.Con name cols)) =
  name : FoldMap NodeFieldNamesF @@ cols

type family NodeFieldNames (tree :: Kind.Tree) :: [FieldId] where
  NodeFieldNames ('Kind.Tree _ _ ('Kind.Prod _ cols)) =
    FoldMap NodeFieldNamesF @@ cols
  NodeFieldNames ('Kind.Tree name _ ('Kind.Sum _ cons)) =
    name : FoldMap ConFieldNames @@ cons
  NodeFieldNames ('Kind.Tree name _ _) =
    '[name]

data NodeFieldNamesF :: Kind.Tree -> Exp [FieldId]
type instance Eval (NodeFieldNamesF tree) = NodeFieldNames tree

type family MkQueryMeta (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: QueryMeta where
  MkQueryMeta ('Kind.Tree name _ _) dTree =
    'QueryMeta dTree name (NodeFieldNamesF @@ dTree)
