module Polysemy.Hasql.Data.Set where

newtype Set a =
  Set { unSet :: a }
  deriving stock (Eq, Show, Generic)
