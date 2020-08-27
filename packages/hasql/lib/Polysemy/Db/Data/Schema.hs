module Polysemy.Db.Data.Schema where

import Hasql.Statement (Statement)

newtype IdQuery =
  IdQuery { id :: UUID }
  deriving (Eq, Show, Generic)

deriveGeneric ''IdQuery

-- TODO make Statement a type param
data Schema q d :: Effect where
  Fetch :: Schema q d m (Statement q (Maybe d))
  FetchAll :: Schema q d m (Statement () [d])
  Insert :: Schema q d m (Statement d ())
  Upsert :: Schema q d m (Statement d ())
  Delete :: Schema q d m (Statement q ())

makeSem ''Schema
