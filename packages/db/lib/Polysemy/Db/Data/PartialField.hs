module Polysemy.Db.Data.PartialField where

data PartialField (a :: Type) =
  Update Text a
  |
  Keep
  deriving (Eq, Show, Functor)

instance Applicative PartialField where
  pure = Update "pure"
  Update _ f <*> Update n a = Update n (f a)
  _ <*> _ = Keep

data FieldPath =
  FieldPath [Symbol]
  |
  FieldName Symbol

data FieldUpdate (name :: FieldPath) (a :: *) =
  FieldUpdate a
  deriving (Eq, Show)
