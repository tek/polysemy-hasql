module Polysemy.Hasql.Test.QueueTest where

import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Data.Uid (Uuid, intUuid)
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Test.Data.Hedgehog (Hedgehog)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))

import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Effect.DbConnectionPool as DbConnectionPool
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Queue.Input (interpretInputDbQueueFull)
import Polysemy.Hasql.Queue.Output (interpretOutputDbQueueFull)
import Polysemy.Hasql.Test.Run (integrationTestWithDb)

data Dat =
  Dat {
    num :: Text
  }
  deriving stock (Eq, Show, Generic)

json ''Dat

prog ::
  ∀ oe t dt r .
  Members [Input (Maybe (Uuid Dat)), Output (Uuid Dat) !! oe, Stop oe, Stop DbError, Time t dt, Hedgehog IO] r =>
  Members [DbConnectionPool !! DbConnectionError, Stop DbConnectionError] r =>
  Sem r ()
prog = do
  Time.sleep (MilliSeconds 500)
  restop (output d1 >> output d2)
  dequeue1 <- input
  restop @DbConnectionError (DbConnectionPool.kill "dequeue-test-queue")
  dequeue2 <- input
  assertJust d1 dequeue1
  assertJust d2 dequeue2
  restop (output d3)
  assertJust d3 =<< input
  -- tag @"test-queue-output" (restop @DbConnectionError (raiseUnder @HasqlConnection DbConnection.kill))
  -- TODO this used to kill the output queue connection, but it's not unique anymore
  -- restop @DbConnectionError (DbConnectionPool.release "dequeue-test-queue")
  restop (output d1)
  assertJust d1 =<< input
  where
    d1 = intUuid 1 (Dat "5'\\'\'\n")
    d2 = intUuid 2 (Dat "bonjour")
    d3 = intUuid 3 (Dat "data")

test_queue :: UnitTest
test_queue =
  integrationTestWithDb \ _ ->
    mapStop @QueueOutputError @Text show $
    interpretOutputDbQueueFull @"test-queue" @(Uuid Dat) $
    interpretInputDbQueueFull @"test-queue" (Seconds 0) def (const (pure True)) $
    prog
