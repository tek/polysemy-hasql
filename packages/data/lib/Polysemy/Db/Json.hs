{-# language NoImplicitPrelude #-}

module Polysemy.Db.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Language.Haskell.TH.Syntax as TH

import Prelude (Bool(True), dropWhile, (==))

basicOptions :: Aeson.Options
basicOptions =
  Aeson.defaultOptions {
    Aeson.fieldLabelModifier = dropWhile ('_' ==)
  }

jsonOptions :: Aeson.Options
jsonOptions =
  basicOptions {
    Aeson.unwrapUnaryRecords = True
  }

defaultJson :: TH.Name -> TH.Q [TH.Dec]
defaultJson =
  deriveJSON jsonOptions
