module Polysemy.Hasql.Data.Crud where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.Partial (Partial)

-- TODO generalize @Statement@ using @Profunctor@?
data Crud q d :: Effect where
  Fetch :: Crud q d m (Statement q (Maybe d))
  FetchAll :: Crud q d m (Statement () [d])
  Insert :: Crud q d m (Statement d ())
  Upsert :: Crud q d m (Statement d ())
  Delete :: Crud q d m (Statement q [d])
  DeleteAll :: Crud q d m (Statement () [d])
  Update :: q -> Partial d -> Crud q d m (Statement () (Maybe d))

makeSem ''Crud
