module Polysemy.Hasql.Data.QueueOutputError where

import Polysemy.Db.Data.DbError (DbError)

data QueueOutputError =
  Insert DbError
  |
  Notify DbError
  deriving (Eq, Show)
