module Polysemy.Hasql.Test.AtomicStateTest where

import Exon (exon)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))

import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.AtomicState (interpretAtomicStateDb)
import Sqel.PgType (MkTableSchema(tableSchema))
import Sqel.Product (prod)
import Sqel.Prim (prims)
import Polysemy.Hasql.Interpreter.Store (interpretDbTable)

data Cat =
  Cat {
    number :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

test_atomicStateDb :: UnitTest
test_atomicStateDb =
  integrationTest $ interpretDbTable ts do
    r <- interpretAtomicStateDb ts (pure (Cat 5 "fuzzyboots")) do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [exon|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
  where
    ts = tableSchema (prod prims)
