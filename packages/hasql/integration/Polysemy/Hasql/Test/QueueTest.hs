module Polysemy.Hasql.Test.QueueTest where

import Polysemy.Async (asyncToIOFinal)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Uid (Uuid, intUuid)
import Polysemy.Input (Input, input)
import Polysemy.Output (Output, output)
import Polysemy.Tagged (Tagged, tag, untag)
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Test.Data.Hedgehog (Hedgehog)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds(MilliSeconds), Seconds(Seconds), Time, interpretTimeGhc)

import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import Polysemy.Hasql.Database (HasqlConnection)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Queue (interpretInputDbQueueFullGen, interpretOutputDbQueueFullGen)
import Polysemy.Hasql.Test.Run (integrationTestWithDb)

data Dat =
  Dat {
    num :: Text
  }
  deriving (Eq, Show, Generic)

defaultJson ''Dat

prog ::
  ∀ oe t dt m r .
  Monad m =>
  Members [Input (Maybe (Uuid Dat)), Output (Uuid Dat) !! oe, Stop oe, Stop DbError, Time t dt, Hedgehog m] r =>
  Members [Tagged "test-queue-input" HasqlConnection, Stop DbConnectionError] r =>
  Member (Tagged "test-queue-output" HasqlConnection) r =>
  Sem r ()
prog = do
  Time.sleep (MilliSeconds 100)
  restop (output d1 >> output d2)
  dequeue1 <- input
  tag @"test-queue-input" (restop @DbConnectionError (raiseUnder @HasqlConnection DbConnection.kill))
  dequeue2 <- input
  assertJust d1 dequeue1
  assertJust d2 dequeue2
  restop (output d3)
  assertJust d3 =<< input
  tag @"test-queue-output" (restop @DbConnectionError (raiseUnder @HasqlConnection DbConnection.kill))
  restop (output d1)
  assertJust d1 =<< input
  where
    d1 =
      intUuid 1 (Dat "5'\\'\'\n")
    d2 =
      intUuid 2 (Dat "bonjour")
    d3 =
      intUuid 3 (Dat "data")

test_queue :: UnitTest
test_queue =
  integrationTestWithDb \ conf ->
    asyncToIOFinal $
    mapStop @QueueOutputError @Text show $
    interpretTimeGhc $
    (interpretDbConnection "test-queue-input" conf . untag @"test-queue-input") $
    (interpretDbConnection "test-queue-output" conf . untag @"test-queue-output") $
    interpretOutputDbQueueFullGen @"test-queue" @(Uuid Dat) $
    interpretInputDbQueueFullGen @"test-queue" (Seconds 0) (const (pure False)) $
    prog
