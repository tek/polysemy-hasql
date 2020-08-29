module Polysemy.Db.Test.InitTest where

import Control.Lens (view)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import qualified Polysemy.Db.Data.DbConnection as DbConnection
import Polysemy.Db.Data.Table (tableName)
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table (initTable, tableColumns)
import Polysemy.Db.Test.Database (withTestTableGen)
import Polysemy.Db.Test.Run (integrationTest)
import Polysemy.Test (UnitTest)
import Polysemy.Test.Hedgehog (assertJust)

data Init =
  Init {
    f1 :: Text
  }
  deriving (Eq, Show)

deriveGeneric ''Init

data InitRep =
  InitRep {
    f1 :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''InitRep

test_initTable :: UnitTest
test_initTable =
  integrationTest do
    withTestTableGen @Init @InitRep $ \(view tableName -> name) -> do
      Right conn <- DbConnection.connect
      initTable conn (TableStructure name newColumns)
      assertJust (columns <> extra) =<< tableColumns conn name
  where
    columns =
      Column "f1" "text" def :| []
    newColumns =
      Columns ((Column "f1" "text" def :| []) <> extra)
    extra =
      Column "f2" "bigint" def :| [Column "f3" "uuid" def]
