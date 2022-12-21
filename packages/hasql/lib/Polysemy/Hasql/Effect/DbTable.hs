module Polysemy.Hasql.Effect.DbTable where

import Hasql.Statement (Statement)
import Sqel.Data.Uid (Uid)

type DbTable :: Type -> Effect
data DbTable a :: Effect where
  -- Schema :: DbTable a m (TableSchema a)
  Statement :: p -> Statement p o -> DbTable a m o

makeSem ''DbTable

type StoreTable i a =
  DbTable (Uid i a)
