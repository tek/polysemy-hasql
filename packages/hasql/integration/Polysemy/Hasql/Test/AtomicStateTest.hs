module Polysemy.Hasql.Test.AtomicStateTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))

import Polysemy.Hasql.AtomicState (interpretAtomicStateDbAsAuto)
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
    r <- interpretAtomicStateDbAsAuto (Cat 5 "fuzzyboots") do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [text|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
