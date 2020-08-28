module Polysemy.Db.Test.InitTest where

import Control.Lens (view)
import Hasql.Session (QueryError)
import Polysemy.Db.Random (runRandomIO)
import Polysemy.Fail (failToEmbed)
import Polysemy.Resource (resourceToIOFinal)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import qualified Polysemy.Db.Data.DbConnection as DbConnection
import Polysemy.Db.Data.Table (tableName)
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.DbConnection (interpretDbConnection)
import Polysemy.Db.Table (initTable, tableColumns)
import Polysemy.Db.Test (UnitTest, assertJust, evalEither)
import Polysemy.Db.Test.Database (withTestTableGen)
import Polysemy.Db.Test.DbConfig (dbConfig)

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
test_initTable = do
  conf <- dbConfig
  r <-
    lift $
    runFinal $
    embedToFinal $
    resourceToIOFinal $
    runError $
    runError @QueryError $
    runRandomIO $
    failToEmbed $
    interpretDbConnection conf $
    withTestTableGen @Init @InitRep $ \(view tableName -> name) -> do
      Right conn <- DbConnection.connect
      initTable conn (TableStructure name newColumns)
      tableColumns conn name
  assertJust (columns <> extra) =<< evalEither =<< evalEither r
  where
    columns =
      Column "f1" "text" def :| []
    newColumns =
      Columns ((Column "f1" "text" def :| []) <> extra)
    extra =
      Column "f2" "bigint" def :| [Column "f3" "uuid" def]
