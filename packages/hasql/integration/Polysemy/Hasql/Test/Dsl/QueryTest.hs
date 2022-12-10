{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.Dsl.QueryTest where

import Generics.SOP (NP (Nil, (:*)))
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
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (Dd, DdK (DdK), (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Term (DdTerm)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs, primNewtype)
import Sqel.Product (prod, prodAs, uid)
import Sqel.Query (checkQuery)
import qualified Sqel.Query.Combinators as Q
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))
import Sqel.ReifyDd (ReifyDd (reifyDd))
import Sqel.Statement (qStatement)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

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
    n :: Text
  }
  deriving stock (Eq, Show, Generic)

newtype TextNt =
  TextNt { unTextNt :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: TextNt
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    po :: Pord
  }
  deriving stock (Eq, Show, Generic)

td :: Dd ('DdK _ _ (Uid Int64 Dat) _)
td =
  uid prim (
    prim :>
    prod (prim :> primNewtype)
  )

codec :: FullCodec (Uid Int64 Dat)
codec =
  reifyCodec td

term :: DdTerm
term =
  reifyDd td

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema td

idSchema :: QuerySchema Int64 (Uid Int64 Dat)
idSchema =
  checkQuery (primAs @"id") td

interpretQuery ::
  Member (Database !! DbError) r =>
  InterpreterFor (Query Q [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretResumable \ (Query params) -> restop (Database.statement params stm)
  where
    stm :: Statement Q [Uid Int64 Dat]
    stm =
      qStatement (checkQuery qd td) (tableSchema td)
    qd =
      prod (
        prodAs @"po" (prim :* Nil) :*
        prod (nullable Q.limit :* nullable Q.offset :* Nil) :*
        primAs @"name" :*
        Nil
      )

test_dslQuery :: UnitTest
test_dslQuery =
  integrationTest do
    interpretDbTable ts $ interpretStoreDb ts idSchema $ interpretQuery do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        for_ @[] [1..10] \ i ->
          Store.insert (Uid i (Dat "name" (Pord i "hnnnggg")))
        r <- fmap (view #id) <$> Query.query (Q (PordQ "hnnnggg") (Par (Just 2) (Just 2)) "name")
        [3, 4] === r
