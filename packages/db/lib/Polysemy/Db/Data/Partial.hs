{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Db.Data.Partial where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Text.Show as Show
import Unsafe.Coerce (unsafeCoerce)

import Polysemy.Db.Data.PartialField (PartialTree, Partially (partially))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial.Update (UpdatePartialTree)

type PartialFor d tree dataTree =
  (
      GenDataTree d dataTree,
      ReifyDataTree dataTree d,
      UpdatePartialTree dataTree tree,
      Partially d tree
  )

newtype Partial (d :: Type) =
  Partial {
    unPartial :: ∀ tree dataTree .
      PartialFor d tree dataTree =>
      PartialTree tree
    }

getPartial ::
  ∀ d tree dataTree .
  PartialFor d tree dataTree =>
  Partial d ->
  PartialTree tree
getPartial (Partial p) =
  unsafeCoerce p

wrapPartial ::
  ∀ d tree dataTree .
  PartialFor d tree dataTree =>
  PartialTree tree ->
  Partial d
wrapPartial t =
  Partial (unsafeCoerce t)

type UidPartial i d =
  Partial (Uid i d)

instance (PartialFor d tree dataTree, Show (PartialTree tree)) => Show (Partial d) where
  show (Partial tree) =
    show tree

instance (PartialFor d tree dataTree, Eq (PartialTree tree)) => Eq (Partial d) where
  Partial l == Partial r =
    l == r

instance (PartialFor d tree dataTree, FromJSON (PartialTree tree)) => FromJSON (Partial d) where
  parseJSON =
    fmap wrapPartial . parseJSON

instance (PartialFor d tree dataTree, ToJSON (PartialTree tree)) => ToJSON (Partial d) where
  toJSON (Partial tree) =
    toJSON tree

partial ::
  ∀ d .
  Partial d
partial =
  Partial (partially @d)
