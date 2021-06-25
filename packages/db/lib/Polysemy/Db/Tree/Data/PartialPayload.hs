{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Db.Tree.Data.PartialPayload (
  PartialPayload,
  UidPartialPayload,
  partialPayload,
  decodePartialPayload,
  decodePartialPayloadTree,
) where

import Data.Aeson (Result (Error, Success), Value, fromJSON)

import Polysemy.Db.Data.PartialField (PartialTree, Partially)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Partial (Partial)

newtype PartialPayload d =
  PartialPayload { unPartialPayload :: Value }
  deriving (Eq, Show)

defaultJson 'PartialPayload

type UidPartialPayload i d =
  PartialPayload (Uid i d)

partialPayload ::
  ∀ d tree .
  Partially d tree =>
  ToJSON (PartialTree tree) =>
  PartialTree tree ->
  PartialPayload d
partialPayload tree =
  PartialPayload (toJSON tree)

decodePartialPayloadTree ::
  ∀ d tree .
  Partially d tree =>
  FromJSON (PartialTree tree) =>
  PartialPayload d ->
  Either Text (PartialTree tree)
decodePartialPayloadTree (PartialPayload val) =
  case fromJSON val of
    Success tree -> Right tree
    Error e -> Left (toText e)

decodePartialPayload ::
  ∀ d tree .
  Partially d tree =>
  FromJSON (Partial d) =>
  PartialPayload d ->
  Either Text (Partial d)
decodePartialPayload (PartialPayload val) =
  case fromJSON val of
    Success tree -> Right tree
    Error e -> Left (toText e)
