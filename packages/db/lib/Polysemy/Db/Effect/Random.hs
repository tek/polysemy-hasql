module Polysemy.Db.Effect.Random where

data Random a :: Effect where
  Random :: Random a m a
  RandomR :: (a, a) -> Random a m a

makeSem ''Random
