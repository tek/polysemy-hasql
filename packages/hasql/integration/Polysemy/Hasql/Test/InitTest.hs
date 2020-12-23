module Polysemy.Hasql.Test.InitTest where

import Control.Lens (view)
import qualified Data.Set as Set
import Polysemy.Db.Data.Column (Prim)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn(ExistingColumn))
import Polysemy.Hasql.Data.Table (tableName)
import Polysemy.Hasql.Table (initTable, tableColumns)
import Polysemy.Hasql.Test.Database (withTestTableGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Data.DbType (Column(Column), DbType(Prod, Prim), unName)

data Init =
  Init {
    f1 :: Text
  }
  deriving (Eq, Show, Generic)

data InitRep =
  InitRep {
    f1 :: Prim
  }
  deriving (Eq, Show, Generic)

test_initTable :: UnitTest
test_initTable =
  integrationTest do
    withTestTableGen @InitRep @Init \ (view tableName -> name) -> do
      restop @DbError @Database $ Database.connect \ connection -> do
        initTable connection (Column name [qt|"#{unName name}"|] "" def (Prod extra))
        assertJust (Set.fromList existing) . fmap (Set.fromList . toList) =<< tableColumns connection name
  where
    extra =
      [Column "f2" [qt|"f2"|] "bigint" def Prim, Column "f3" [qt|"f3"|] "uuid" def Prim]
    existing =
      [ExistingColumn "f1" "text", ExistingColumn "f2" "bigint", ExistingColumn "f3" "uuid"]
