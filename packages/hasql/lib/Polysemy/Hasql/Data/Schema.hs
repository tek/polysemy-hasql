module Polysemy.Hasql.Data.Schema where

import Hasql.Statement (Statement)

-- TODO rename to @CrudStatement@ or something, generalize @Statement@?
data Schema q d :: Effect where
  Fetch :: Schema q d m (Statement q (Maybe d))
  FetchAll :: Schema q d m (Statement () [d])
  Insert :: Schema q d m (Statement d ())
  Upsert :: Schema q d m (Statement d ())
  Delete :: Schema q d m (Statement q [d])
  DeleteAll :: Schema q d m (Statement () ())

makeSem ''Schema
