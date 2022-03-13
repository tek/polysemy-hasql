module Polysemy.Hasql.Test.DeleteTest where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (Auto)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Test.Database (withTestStore)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

prog ::
  Member (Store () Dat) r =>
  Sem r ()
prog =
  void $ Store.deleteAll

test_deleteEmpty :: UnitTest
test_deleteEmpty = do
  integrationTest do
    withTestStore @Auto @Auto do
      restop @DbError prog
