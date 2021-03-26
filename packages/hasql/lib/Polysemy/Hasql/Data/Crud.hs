module Polysemy.Hasql.Data.Crud where

import Hasql.Statement (Statement)

-- TODO generalize @Statement@?
data Crud q d :: Effect where
  Fetch :: Crud q d m (Statement q (Maybe d))
  FetchAll :: Crud q d m (Statement () [d])
  Insert :: Crud q d m (Statement d ())
  Upsert :: Crud q d m (Statement d ())
  Delete :: Crud q d m (Statement q [d])
  DeleteAll :: Crud q d m (Statement () [d])

makeSem ''Crud