 {-# OPTIONS_GHC -fclear-plugins #-}

module Polysemy.Db.Data.DbHost where

newtype DbHost =
  DbHost Text
  deriving (Eq, Show)
  deriving newtype (IsString)

instance Default DbHost where
  def =
    "localhost"

defaultJson ''DbHost
