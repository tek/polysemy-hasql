module Polysemy.Hasql.Test.InitTest where

import Control.Lens (view)
import qualified Data.Set as Set
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Resume (restop)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.TableStructure (Column(Column), TableStructure(TableStructure))
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.Table (tableName)
import Polysemy.Hasql.Table (initTable, tableColumns)
import Polysemy.Hasql.Test.Database (withTestTableGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)

data Init =
  Init {
    f1 :: Text
  }
  deriving (Eq, Show, Generic)

data InitRep =
  InitRep {
    f1 :: Prim Auto
  }
  deriving (Eq, Show, Generic)

test_initTable :: UnitTest
test_initTable =
  integrationTest do
    withTestTableGen @InitRep @Init \ (view tableName -> name) -> do
      restop @DbError @Database $ Database.connect \ connection -> do
        initTable connection (TableStructure name (toList extra))
        assertJust (Set.fromList (extra <> columns)) . fmap (Set.fromList . toList) =<< tableColumns connection name
  where
    columns =
      [Column "f1" "text" def Nothing]
    extra =
      [Column "f2" "bigint" def Nothing, Column "f3" "uuid" def Nothing]
