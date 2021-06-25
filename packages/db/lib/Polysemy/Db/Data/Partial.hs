{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Db.Data.Partial where

import qualified Text.Show as Show
import Unsafe.Coerce (unsafeCoerce)

import Polysemy.Db.Data.PartialField (PartialTree, Partially (partially))
import Polysemy.Db.Data.Uid (Uid)

data Partial (d :: Type) =
  Partial { unPartial :: ∀ tree . Partially d tree => PartialTree tree }

getPartial ::
  Partially d tree =>
  Partial d ->
  PartialTree tree
getPartial =
  unsafeCoerce . unPartial

wrapPartial ::
  Partially d tree =>
  PartialTree tree ->
  Partial d
wrapPartial t =
  Partial (unsafeCoerce t)

type UidPartial i d =
  Partial (Uid i d)

instance (Partially d tree, Show (PartialTree tree)) => Show (Partial d) where
  show (Partial tree) =
    show tree

instance (Partially d tree, Eq (PartialTree tree)) => Eq (Partial d) where
  Partial l == Partial r =
    l == r

instance (Partially d tree, FromJSON (PartialTree tree)) => FromJSON (Partial d) where
  parseJSON value =
    parseJSON @(PartialTree tree) value <&> \case
      tree -> Partial (unsafeCoerce tree)

instance (Partially d tree, ToJSON (PartialTree tree)) => ToJSON (Partial d) where
  toJSON (Partial tree) =
    toJSON tree

partial ::
  ∀ d .
  Partial d
partial =
  Partial (partially @d)
