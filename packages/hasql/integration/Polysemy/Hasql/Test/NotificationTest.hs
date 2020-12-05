module Polysemy.Hasql.Test.NotificationTest where

import Polysemy.Async (asyncToIOFinal)
import Polysemy.Input (Input, input)
import Polysemy.Output (Output, output)
import Polysemy.Resume (resumeAs)
import Polysemy.Tagged (untag)
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Test.Data.Hedgehog (Hedgehog)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Queue (interpretInputDbQueueFull, interpretOutputDbQueue)
import Polysemy.Hasql.Test.Run (integrationTestWithDb)
import Polysemy.Resume (resume, type (!))
import Polysemy.Time (Time, interpretTimeGhc)

data Dat =
  Dat {
    num :: Text
  }
  deriving (Eq, Show)

defaultJson ''Dat

prog ::
  Monad m =>
  Members [Input (Maybe Dat) ! DbError, Output Dat ! DbError, Time t dt, Hedgehog m] r =>
  Sem r ()
prog = do
  resume (output d1 >> output d2) dbgs
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
    (interpretDbConnection conf . untag @"test-queue-input") $
    (interpretDbConnection conf . untag @"test-queue") $
    interpretOutputDbQueue @"test-queue" $
    interpretInputDbQueueFull @"test-queue" @"test-queue-input" throw prog
