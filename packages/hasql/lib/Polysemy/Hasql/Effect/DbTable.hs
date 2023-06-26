module Polysemy.Hasql.Effect.DbTable where

import Polysemy.Db.Data.DbError (DbError)
import Sqel (ResultShape, Statement, Uid)

type DbTable :: Type -> Effect
data DbTable a :: Effect where
  WithTable :: Statement (a : tables) query proj -> (Statement tables query proj -> m x) -> DbTable a m x
  Statement :: ResultShape proj result => Bool -> query -> Statement '[a] query proj -> DbTable a m result

makeSem ''DbTable

type DbTables :: [Type] -> [Effect]
type family DbTables as where
  DbTables '[] = '[]
  DbTables (a : as) = DbTable a !! DbError : DbTables as

type StoreTable i a =
  DbTable (Uid i a)

type StoreTables :: Type -> [Type] -> [Effect]
type family StoreTables i as where
  StoreTables _ '[] = '[]
  StoreTables i (a : as) = StoreTable i a !! DbError : StoreTables i as
