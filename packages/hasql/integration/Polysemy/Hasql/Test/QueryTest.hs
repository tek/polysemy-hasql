module Polysemy.Hasql.Test.QueryTest where

import Hasql.Encoders (Params)

import Polysemy.Db.Data.Column (Auto, NewtypePrim, Prim, PrimaryKey)
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.Schema (IdQuery)
import Polysemy.Hasql.Query.Many (interpretManyWith)
import Polysemy.Hasql.Query.One (interpretOneWith)
import Polysemy.Hasql.Table.QueryParams (queryParams)
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Test.Database (withTestStoreTableGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, (===))
import Polysemy.Test.Hedgehog (assertJust)

newtype Content =
  Content { unContent :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data Dat =
  Dat {
     id :: UUID,
     content :: Content,
     number :: Int
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    id :: Prim PrimaryKey,
    content :: NewtypePrim Auto,
    number :: Prim Auto
  }
  deriving (Eq, Show, Generic)

data ContentNumber =
  ContentNumber {
    content :: Content,
    number :: Maybe (LessOrEq Int)
  }
  deriving (Eq, Show, Generic)

target :: Dat
target =
  Dat (Uid.uuid 2) "hello" 5

prog ::
  Members [Error (StoreError DbError), StoreQuery ContentNumber DbError [Dat]] r =>
  Members [Store IdQuery DbError Dat, StoreQuery ContentNumber DbError (Maybe Dat)] r =>
  Sem r (Int, Maybe Dat)
prog = do
  Store.insert (Dat (Uid.uuid 1) "hello" 10)
  Store.insert target
  Store.insert (Dat (Uid.uuid 3) "goodbye" 1)
  Store.insert (Dat (Uid.uuid 4) "goodbye" 5)
  r1 :: [Dat] <- StoreQuery.basicQuery (ContentNumber "hello" Nothing)
  r2 <- StoreQuery.basicQuery (ContentNumber "hello" (Just 6))
  pure (length r1, r2)

queryParamsContentNumber :: Params ContentNumber
queryParamsContentNumber =
  queryParams @(Rep ContentNumber) @ContentNumber

test_query :: UnitTest
test_query = do
  integrationTest do
    withTestStoreTableGen @DatRep \ table ->
      interpretOneWith @DatRep (table ^. QueryTable.structure) $
      interpretManyWith @DatRep (table ^. QueryTable.structure) do
        (count, result) <- prog
        count === 2
        assertJust target result
