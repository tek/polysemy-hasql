module Polysemy.Db.Data.IdQuery where

data IdQuery i =
  IdQuery { id :: i }
  deriving (Eq, Show, Generic)

type UuidQuery =
  IdQuery UUID
