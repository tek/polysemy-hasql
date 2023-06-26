module Polysemy.Hasql.Test.AtomicStateTest where

import Exon (exon)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))
import Sqel (Gen, Sqel, Table, sqel)

import Polysemy.Hasql.Interpreter.AtomicState (interpretAtomicStateDb)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Cat =
  Cat {
    number :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Cat = Table "cat" Cat Gen

table_Cat :: Sqel Table_Cat
table_Cat = sqel

test_atomicStateDb :: UnitTest
test_atomicStateDb =
  integrationTest $ interpretTable table_Cat do
    r <- interpretAtomicStateDb table_Cat (pure (Cat 5 "fuzzyboots")) do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [exon|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
