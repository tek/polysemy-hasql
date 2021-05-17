module Polysemy.Db.Tree.Partial.Update where

import Generics.SOP (I(I), NP ((:*)), NS (Z, S))

import Polysemy.Db.Data.Column (Auto)
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Tree(tree))
import Polysemy.Db.Tree.Data (DataCon, DataParams, DataTree)
import Polysemy.Db.Tree.Data.TreeMeta (TM(TM), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Partial.Insert (PartialCon, PartialTree)
import qualified Polysemy.Db.Type.Data.Tree as Type

class UpdatePartialProd (ds :: [Kind.Tree]) (us :: [Kind.Tree]) where
  updatePartialProd :: NP DataTree ds -> NP PartialTree us -> NP DataTree ds

instance UpdatePartialProd '[] '[] where
  updatePartialProd =
    const

instance (
    UpdatePartialTree d u,
    UpdatePartialProd ds us
  ) => UpdatePartialProd (d : ds) (u : us) where
    updatePartialProd (d :* ds) (u :* us) =
      updatePartialTree d u :* updatePartialProd ds us

class UpdatePartialSum (ds :: [Kind.Con]) (us :: [Kind.Con]) where
  updatePartialSum :: NS DataCon ds -> NS PartialCon us -> NS DataCon ds

class UpdatePartialSumProd (ds :: [Kind.Con]) (us :: [Kind.Con]) where
  updatePartialSumProd :: NS DataCon ds -> NP PartialCon us -> NS DataCon ds

instance UpdatePartialSumProd '[] us where
  updatePartialSumProd =
    const

instance (
    UpdatePartialProd d u,
    UpdatePartialSumProd ds us
  ) => UpdatePartialSumProd ('Kind.Con _n d : ds) ('Kind.Con _n u : us) where
  updatePartialSumProd (Z (Type.Con d)) ((Type.Con u) :* _) =
    Z (Type.Con (updatePartialProd d u))
  updatePartialSumProd (S d) (_ :* us) =
    S (updatePartialSumProd d us)

class UpdatePartialTree (dataTree :: Kind.Tree) (updateTree :: Kind.Tree) where
  updatePartialTree :: DataTree dataTree -> PartialTree updateTree -> DataTree dataTree

updateNode :: I d -> PartialField d -> I d
updateNode old = \case
  PartialField.Update _ new ->
    pure new
  PartialField.Keep ->
    old

instance UpdatePartialTree ('Kind.Tree name eff ('Kind.Prim d)) ('Kind.Tree name eff ('Kind.Prim d)) where
  updatePartialTree (Type.Tree t (Type.Prim old)) (Type.Tree _ (Type.Prim new)) =
    Type.Tree t (Type.Prim (updateNode old new))

instance (
    UpdatePartialSum subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.Sum d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Sum old subData)) (Type.Tree () (Type.Sum new subUpdate)) =
    Type.Tree () (Type.Sum (updateNode old new) (updatePartialSum subData subUpdate))

instance (
    UpdatePartialProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Prod d subData)) ('Kind.Tree name eff ('Kind.Prod d subUpdate)) where
  updatePartialTree (Type.Tree () (Type.Prod old subData)) (Type.Tree () (Type.Prod new subUpdate)) =
    Type.Tree () (Type.Prod (updateNode old new) (updatePartialProd subData subUpdate))

instance (
    meta ~ 'TreeMeta name Auto d,
    Tree DataParams meta ('Kind.Tree name eff ('Kind.Sum d subData)),
    UpdatePartialSumProd subData subUpdate
  ) => UpdatePartialTree ('Kind.Tree name eff ('Kind.Sum d subData)) ('Kind.Tree name eff ('Kind.SumProd d subUpdate)) where
    updatePartialTree (Type.Tree () (Type.Sum old subData)) (Type.Tree () (Type.SumProd PartialField.Keep subUpdate)) =
      Type.Tree () (Type.Sum old (updatePartialSumProd subData subUpdate))
    updatePartialTree (Type.Tree () (Type.Sum _ _)) (Type.Tree () (Type.SumProd (PartialField.Update _ new) _)) =
      tree @DataParams @meta (TM (I new))
