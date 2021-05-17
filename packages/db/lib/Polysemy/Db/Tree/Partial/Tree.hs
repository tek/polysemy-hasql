module Polysemy.Db.Tree.Partial.Tree where

import Polysemy.Db.Data.Column (Auto)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Tree (Root(root))
import Polysemy.Db.Tree.Api (TreePrim(..))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import qualified Polysemy.Db.Type.Data.Tree as Type

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField
type PartialCon = Type.Con () PartialField
type PartialParams = 'Params PartialTag () PartialField 'True

instance TreePrim PartialTag PartialField name d where
  treePrim _ =
    PartialField.Keep

instance TreeEffects DefaultEffects rep d effs => TreeEffects PartialTag rep d effs where

class Partial d tree | d -> tree where
  partial :: PartialTree tree

instance (
    Root Auto PartialParams d tree
  ) => Partial d tree where
  partial =
    root @Auto @PartialParams @d PartialField.Keep
