module Polysemy.Db.Effect.Id where

data Id i :: Effect where
  NewId :: Id i m i

makeSem ''Id
