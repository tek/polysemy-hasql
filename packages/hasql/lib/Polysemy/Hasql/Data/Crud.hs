module Polysemy.Hasql.Data.Crud where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.Partial (Partial)
import Polysemy.Db.Data.Uid (Uid)

import Polysemy.Hasql.Table.ResultShape (ResultShape)

data Crud i d q p :: Effect where
  Fetch :: ResultShape d o => Crud i d q p m (Statement i o)
  FetchAll :: Crud i d q p m (Statement () [d])
  FetchQ :: ResultShape d o => Crud i d q p m (Statement q o)
  Insert :: Crud i d q p m (Statement d ())
  Upsert :: Crud i d q p m (Statement d ())
  Delete :: Crud i d q p m (Statement i [d])
  DeleteAll :: Crud i d q p m (Statement () [d])
  Update :: i -> Partial p -> Crud i d q p m (Statement () (Maybe d))
  UpdateQ :: q -> Partial p -> Crud i d q p m (Statement () (Maybe d))

makeSem ''Crud

type UidCrud i d =
  Crud i (Uid i d) i d
