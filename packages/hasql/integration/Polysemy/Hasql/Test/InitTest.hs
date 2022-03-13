module Polysemy.Hasql.Test.InitTest where

import Control.Lens (view)
import qualified Data.Set as Set
import Data.UUID (UUID)
import Exon (exon)
import Polysemy.Db.Data.ColumnOptions (notNull)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (Prim)
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.DbType (Column (Column), DbType (Prim, Prod), Selector (Selector), unName)
import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Table (initTable, tableColumns)
import Polysemy.Hasql.Test.Database (withTestTableGen)
import Polysemy.Hasql.Test.Run (integrationTest)

data Init =
  Init {
    f1 :: Text
  }
  deriving stock (Eq, Show, Generic)

data InitRep =
  InitRep {
    f1 :: Prim
  }
  deriving stock (Eq, Show, Generic)

data Res =
  Res {
    f1 :: Text,
    f2 :: Maybe Int,
    f3 :: Maybe UUID
  }
  deriving stock (Eq, Show, Generic)

test_initTable :: UnitTest
test_initTable =
  integrationTest do
    withTestTableGen @InitRep @Init \ (view Table.name -> name) -> do
      let n = SqlCode (unName name)
      restop @DbError @Database $ Database.connect \ connection -> do
        () <- Database.sql (Init "a") [exon|insert into "#{n}" (f1) values ($1)|]
        initTable connection (Column name [exon|"#{Selector n}"|] "" def (Prod extra))
        assertJust (Set.fromList existing) . fmap (Set.fromList . toList) =<< tableColumns connection name
        assertJust (Res "a" Nothing Nothing) =<< Database.sql () [exon|select * from "#{n}"|]
  where
    extra =
      [
        Column "f2" [exon|"f2"|] "bigint" def { notNull = False } Prim,
        Column "f3" [exon|"f3"|] "uuid" def { notNull = False } Prim
      ]
    existing =
      [
        ExistingColumn "f1" "text",
        ExistingColumn "f2" "bigint",
        ExistingColumn "f3" "uuid"
      ]
