module Polysemy.Hasql.Data.Schema where

import Hasql.Statement (Statement)

data IdQuery i =
  IdQuery { id :: i }
  deriving (Eq, Show, Generic)

type UuidQuery =
  IdQuery UUID

-- TODO make Statement a type param
data Schema q d :: Effect where
  Fetch :: Schema q d m (Statement q (Maybe d))
  FetchAll :: Schema q d m (Statement () [d])
  Insert :: Schema q d m (Statement d ())
  Upsert :: Schema q d m (Statement d ())
  Delete :: Schema q d m (Statement q ())

makeSem ''Schema
