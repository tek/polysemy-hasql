 {-# OPTIONS_GHC -fclear-plugins #-}

module Polysemy.Db.Data.DbUser where

newtype DbUser =
  DbUser Text
  deriving (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbUser
