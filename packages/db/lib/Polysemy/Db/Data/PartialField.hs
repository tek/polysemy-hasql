module Polysemy.Db.Data.PartialField where

data PartialField (a :: Type) =
  Update Text a
  |
  Keep
  deriving (Eq, Show)

data FieldUpdate (name :: Symbol) (a :: *) =
  FieldUpdate a
  deriving (Eq, Show)
