module Polysemy.Hasql.Table.Enum where

import qualified Data.Map.Strict as Map
import Hasql.Decoders (Value, enum)

import Polysemy.Hasql.SOP.Enum (EnumTable(enumTable))

enumDecodeValue ::
  EnumTable a =>
  Value a
enumDecodeValue =
  enum (`Map.lookup` enumTable)
