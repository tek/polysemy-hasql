{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Db.Tree.Data.PartialPayload (
  PartialPayload,
  UidPartialPayload,
  partialPayload
) where

import Data.Aeson (Value)

import Polysemy.Db.Data.PartialField (PartialTree, Partially)
import Polysemy.Db.Data.Uid (Uid)

newtype PartialPayload d =
  PartialPayload { unPartialPayload :: Value }
  deriving (Eq, Show)

defaultJson 'PartialPayload

type UidPartialPayload i d =
  PartialPayload (Uid i d)

partialPayload ::
  Partially d tree =>
  ToJSON (PartialTree tree) =>
  PartialTree tree ->
  PartialPayload d
partialPayload tree =
  PartialPayload (toJSON tree)
