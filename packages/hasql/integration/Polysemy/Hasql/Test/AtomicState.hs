module Polysemy.Hasql.Test.AtomicState where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))

import Polysemy.Hasql.AtomicState (interpretAtomicStateDb)
import Polysemy.Hasql.ManagedTable (interpretManagedTableAuto)
import Polysemy.Hasql.Query (interpretQueryAuto)
import Polysemy.Hasql.Test.Run (integrationTest)

data Cat =
  Cat {
    number :: Int,
    name :: Text
  }
  deriving (Eq, Show, Generic)

test_atomicStateDb :: UnitTest
test_atomicStateDb =
  integrationTest do
    r <- interpretManagedTableAuto $ interpretQueryAuto $ interpretAtomicStateDb (Cat 5 "fuzzyboots") do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [qt|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
