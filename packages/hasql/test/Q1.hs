{-# LANGUAGE DeriveAnyClass #-}

module Q1 where

import qualified Generics.SOP as SOP

data Q1 =
  Q1 {
    fieldOne :: Text,
    fieldThree :: Double
  }
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
