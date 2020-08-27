module Polysemy.Db.Data.Id where

data Id i :: Effect where
  Generate :: Id i m i

makeSem ''Id
