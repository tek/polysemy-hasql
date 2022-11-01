module Polysemy.Hasql.Effect.Transaction where

type Transaction :: Type -> Effect
data Transaction res :: Effect where
  Resource :: Transaction res m res
  Abort :: Transaction res m a

makeSem ''Transaction

type Transactions res =
  Scoped_ (Transaction res)
