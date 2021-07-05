{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Db.Data.PartialUpdate where

import qualified Text.Show as Show
import Unsafe.Coerce (unsafeCoerce)

import Polysemy.Db.Tree.Partial.Insert (FieldSpec, InsertPaths, PartialTree)

newtype PartialUpdate (d :: Type) (paths :: [FieldSpec]) =
  PartialUpdate { unPartialUpdate :: ∀ tree . InsertPaths d paths tree => PartialTree tree }

getPartialUpdate ::
  ∀ d paths tree .
  InsertPaths d paths tree =>
  PartialUpdate d paths ->
  PartialTree tree
getPartialUpdate p =
  unsafeCoerce (unPartialUpdate p)

wrapPartial ::
  ∀ d paths tree .
  InsertPaths d paths tree =>
  PartialTree tree ->
  PartialUpdate d paths
wrapPartial t =
  PartialUpdate (unsafeCoerce t)

instance (InsertPaths d paths tree, Show (PartialTree tree)) => Show (PartialUpdate d paths) where
  show (PartialUpdate tree) =
    show tree

instance (InsertPaths d paths tree, FromJSON (PartialTree tree)) => FromJSON (PartialUpdate d paths) where
  parseJSON value =
    parseJSON @(PartialTree tree) value <&> \case
      tree -> PartialUpdate (unsafeCoerce tree)
