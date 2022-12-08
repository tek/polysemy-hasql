{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.QueryTest where

import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query (Query))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Column (nullable)
import Sqel.Data.Dd (Dd, DdK (DdK), (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Names (named)
import Sqel.PgType (fullProjection, tableSchema)
import Sqel.Prim (ignore, prim, primAs, primNewtype)
import Sqel.Product (prod, prodAs)
import Sqel.Query (checkQuery)
import qualified Sqel.Query.Combinators as Q
import Sqel.Statement (selectWhere)
import Sqel.Uid (uid)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Query (interpretQueryDd)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

newtype TextNt =
  TextNt { unTextNt :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data PordQ =
  PordQ {
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Par =
  Par {
    limit :: Maybe Int64,
    offset :: Maybe Int64
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    pr :: PordQ,
    params :: Par,
    n :: TextNt,
    ig :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: Maybe TextNt
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    po :: Pord
  }
  deriving stock (Eq, Show, Generic)

ddUidDat :: Dd ('DdK _ _ (Uid Int64 Dat) _)
ddUidDat =
  uid prim (prod (
    prim :>
    prod (prim :> nullable primNewtype)
  ))

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema ddUidDat

queryIdDat :: QuerySchema Int64 (Uid Int64 Dat)
queryIdDat =
  checkQuery (primAs @"id") ddUidDat

interpretQuery ::
  Member (Database !! DbError) r =>
  InterpreterFor (Query Q [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretResumable \ (Query params) -> restop (Database.statement params stm)
  where
    stm :: Statement Q [Uid Int64 Dat]
    stm =
      selectWhere (checkQuery qd ddUidDat) (fullProjection ddUidDat)
    qd =
      prod (
        prodAs @"po" prim :>
        prod (nullable Q.limit :> nullable Q.offset) :>
        named @"name" primNewtype :>
        ignore
      )

inserts ::
  Member (Store Int64 Dat) r =>
  Sem r ()
inserts =
  for_ @[] [1..10] \ i ->
    Store.insert (Uid i (Dat "name" (Pord i (Just (TextNt "hnnnggg")))))

test_query :: UnitTest
test_query =
  integrationTest do
    interpretTable ts $ interpretStoreDb ts queryIdDat $ interpretQuery do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        inserts
        r <- fmap (view #id) <$> Query.query (Q (PordQ "hnnnggg") (Par (Just 2) (Just 2)) "name" Nothing)
        [3, 4] === r

test_queryId :: UnitTest
test_queryId =
  integrationTest do
    interpretTable ts $ interpretStoreDb ts queryIdDat $ interpretQueryDd @[Int64] ddUidDat (primAs @"id" @Int64) (primAs @"id" @Int64) do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        inserts
        r <- Query.query (5 :: Int64)
        [5] === r
