module Sqel.Data.Create where

newtype Create a =
  Create { unSelect :: a }
  deriving stock (Eq, Show)
