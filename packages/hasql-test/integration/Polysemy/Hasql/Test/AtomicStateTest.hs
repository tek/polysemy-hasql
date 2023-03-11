module Polysemy.Hasql.Test.AtomicStateTest where

import Exon (exon)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))
import Sqel (TableSchema)
import Sqel.PgType (MkTableSchema (tableSchema))
import Sqel (prims)
import Sqel (prod)

import Polysemy.Hasql.Interpreter.AtomicState (interpretAtomicStateDb)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Cat =
  Cat {
    number :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

test_atomicStateDb :: UnitTest
test_atomicStateDb =
  integrationTest $ interpretTable ts do
    r <- interpretAtomicStateDb ts (pure (Cat 5 "fuzzyboots")) do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [exon|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
  where
    ts :: TableSchema Cat
    ts = tableSchema (prod prims)
