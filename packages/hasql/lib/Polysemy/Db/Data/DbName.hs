 {-# OPTIONS_GHC -fclear-plugins #-}

module Polysemy.Db.Data.DbName where

newtype DbName =
  DbName Text
  deriving (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbName
