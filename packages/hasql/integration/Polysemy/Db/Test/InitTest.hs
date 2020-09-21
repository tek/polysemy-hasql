module Polysemy.Db.Test.InitTest where

import Control.Lens (view)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.TableStructure (Column(Column), TableStructure(TableStructure))
import Polysemy.Db.Test.Run (integrationTest)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.Table (tableName)
import Polysemy.Hasql.Table (initTable, tableColumns)
import Polysemy.Hasql.Test.Database (withTestTableGen)
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
      Right conn <- DbConnection.connect
      initTable conn (TableStructure name (toList extra))
      assertJust (extra <> columns) =<< tableColumns conn name
  where
    columns =
      [Column "f1" "text" def Nothing]
    extra =
      [Column "f2" "bigint" def Nothing, Column "f3" "uuid" def Nothing]
