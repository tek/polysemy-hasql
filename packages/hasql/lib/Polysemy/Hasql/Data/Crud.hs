module Polysemy.Hasql.Data.Crud where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.Uid (Uid)

import Polysemy.Hasql.Table.ResultShape (ResultShape)

data Crud i d q :: Effect where
  Fetch :: ResultShape d o => Crud i d q m (Statement i o)
  FetchAll :: Crud i d q m (Statement () [d])
  FetchQ :: ResultShape d o => Crud i d q m (Statement q o)
  Insert :: Crud i d q m (Statement d ())
  Upsert :: Crud i d q m (Statement d ())
  Delete :: Crud i d q m (Statement i [d])
  DeleteAll :: Crud i d q m (Statement () [d])

makeSem ''Crud

type UidCrud i d =
  Crud i (Uid i d) i
