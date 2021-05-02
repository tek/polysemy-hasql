module Polysemy.Db.Data.PartialField where

data PartialField (a :: Type) =
  Update Text a
  |
  Keep
  deriving (Eq, Show)

data FieldPath =
  FieldPath [Symbol]
  |
  FieldName Symbol

data FieldUpdate (name :: FieldPath) (a :: *) =
  FieldUpdate a
  deriving (Eq, Show)
