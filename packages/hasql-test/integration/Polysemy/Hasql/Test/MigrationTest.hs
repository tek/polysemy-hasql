module Polysemy.Hasql.Test.MigrationTest where

import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, assertEq)
import Prelude hiding (Default)
import Sqel (
  Def,
  Default,
  Delete,
  Gen,
  IntTable,
  Nullable,
  Prim,
  PrimAs,
  Prod,
  Query,
  Rename,
  RenameType,
  Sqel,
  Uid (Uid),
  query_Int,
  sqel,
  )
import Sqel.Migration (Migrate, (-->))

import Polysemy.Hasql.Data.MigrateSem (MigrateSem)
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Interpreter.DbTable (interpretTableMigrations, interpretTables)
import Polysemy.Hasql.Interpreter.Query (interpretQuery)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data PordOld =
  PordOld {
    p1 :: Int64
  }
  deriving stock (Eq, Show, Generic)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data Dat0 =
  Dat0 {
    old :: Text,
    names :: [Text]
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat0 = IntTable "dat" Dat0 (Prod [Delete Prim, Prim])

table_Dat0 :: Sqel Table_Dat0
table_Dat0 = sqel

data Dat1 =
  Dat1 {
    size :: Int64,
    names :: [Text],
    pordOld :: PordOld
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat1 = IntTable "dat" Dat1  (Prod [Delete (Default "0" Prim), Prim, Prod '[Default "53" Prim]])

table_Dat1 :: Sqel Table_Dat1
table_Dat1 = sqel

data Dat2 =
  Dat2 {
    number :: Int64,
    names :: [Text],
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat2 =
  IntTable "dat" Dat2  (Prod [
    Default "15" Prim,
    Prim,
    Rename "pordOld" (RenameType "sqel_type__PordOld" (Prod [
      Default "53" Prim,
      Nullable Prim
    ]))
  ])

table_Dat2 :: Sqel Table_Dat2
table_Dat2 = sqel

data Dat =
  Dat {
    name :: Text,
    names :: [Text],
    num :: Int64,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat =
  IntTable "dat" Dat (Prod [
    Default "'vunqach'" Prim,
    Prim,
    Rename "number" Prim,
    Gen
  ])

table_Dat :: Sqel Table_Dat
table_Dat = sqel

data Q = Q { nom :: Text }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod '[PrimAs "name"])

query_Q :: Sqel Query_Q
query_Q = sqel

migrations ::
  Migrate Def (MigrateSem r) [Table_Dat, Table_Dat2, Table_Dat1, Table_Dat0]
migrations =
  table_Dat0 --> table_Dat1 --> table_Dat2 --> table_Dat

test_migration :: UnitTest
test_migration =
  integrationTest do
    interpretTables table_Dat1 $ interpretStoreDb query_Int table_Dat1 $ restop @DbError do
      Store.insert (Uid 3 (Dat1 11 ["0"] (PordOld 93)))
      Store.insert (Uid 4 (Dat1 55 ["0", "1"] (PordOld 78)))
    restop @DbError Database.release
    restop @DbError Database.resetInit
    interpretTableMigrations migrations $
      interpretStoreDb query_Int table_Dat $
      interpretQuery @[_] query_Q table_Dat $
      restop @DbError @(Query.Query _ _) $
      restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "1" ["11"] 5 (Pord 10 (Just "new 1"))))
        Store.insert (Uid 2 (Dat "2" ["22", "33"] 5 (Pord 10 (Just "new 2"))))
        assertEq [3, 4] =<< fmap (view #id) <$> Query.query (Q "vunqach")
        assertEq [2] =<< fmap (view #id) <$> Query.query (Q "2")
