module Polysemy.Hasql.Test.NotificationTest where

import Polysemy.Async (asyncToIOFinal)
import Polysemy.Input (Input, input)
import Polysemy.Output (Output, output)
import Polysemy.Resume (Stop, restop, resumeAs, type (!))
import Polysemy.Tagged (untag)
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Test.Data.Hedgehog (Hedgehog)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds(MilliSeconds), Time, interpretTimeGhc)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Queue (interpretInputDbQueueFull, interpretOutputDbQueueFull)
import Polysemy.Hasql.Test.Run (integrationTestWithDb)

data Dat =
  Dat {
    num :: Text
  }
  deriving (Eq, Show)

defaultJson ''Dat

prog ::
  Monad m =>
  Members [Input (Maybe Dat) ! DbError, Output Dat ! DbError, Stop DbError, Time t dt, Hedgehog m] r =>
  Sem r ()
prog = do
  Time.sleep (MilliSeconds 100)
  restop (output d1 >> output d2)
  dequeue1 <- resumeAs Nothing input
  dequeue2 <- resumeAs Nothing input
  assertJust d1 dequeue1
  assertJust d2 dequeue2
  where
    d1 =
      Dat "5'\\'\'\n"
    d2 =
      Dat "bonjour"

test_notification :: UnitTest
test_notification =
  integrationTestWithDb \ conf ->
    asyncToIOFinal $
    interpretTimeGhc $
    (interpretDbConnection "test-queue-input" conf . untag @"test-queue-input") $
    (interpretDbConnection "test-queue" conf . untag @"test-queue") $
    interpretOutputDbQueueFull @"test-queue" $
    interpretInputDbQueueFull @"test-queue" @"test-queue-input" throw prog
