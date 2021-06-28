module Polysemy.Hasql.Test.AtomicStateTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, (===))

import Polysemy.Hasql.AtomicState (interpretAtomicStateDbAuto)
import Polysemy.Hasql.Test.Run (integrationTest)

data Cat =
  Cat {
    number :: Int,
    name :: Text
  }
  deriving (Eq, Show, Generic)

-- TODO
-- the problem here is probably that when deleting all, nonNullable is not used.
-- the same is likely the cause of the similar error in bodhi
--
-- update: it looks rather like instead of returning the qualified fields, the query is returning "payload", mismatching
-- with the expected decoder. needs to return ("payload")."number" etc.
test_atomicStateDb :: UnitTest
test_atomicStateDb =
  integrationTest do
    r <- interpretAtomicStateDbAuto (Cat 5 "fuzzyboots") do
      restop @DbError do
        atomicModify' \ (Cat _ nam) -> Cat 200 [text|mr. #{nam}|]
        atomicGet
    Cat 200 "mr. fuzzyboots" === r
