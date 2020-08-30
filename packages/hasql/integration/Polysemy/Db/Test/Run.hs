module Polysemy.Db.Test.Run where

import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Polysemy.Fail (Fail, failToEmbed)
import Polysemy.Resource (Resource, resourceToIO)
import qualified Polysemy.Test as Hedgehog
import Polysemy.Test (Hedgehog, Test, runTestAuto)
import Polysemy.Test.Data.TestError (TestError)

import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Db.Random (Random, runRandomIO)
import Polysemy.Db.Test.DbConfig (dbConfig)

type TestEffects =
  [
    DbConnection Connection,
    Fail,
    Random,
    Error (StoreError DbError),
    Error DbError,
    Error QueryError,
    Error Text,
    Resource,
    Test,
    Error TestError,
    Embed IO,
    Hedgehog,
    Embed (TestT IO),
    Final (TestT IO)
  ]

integrationTest ::
  Sem TestEffects () ->
  TestT IO ()
integrationTest thunk = do
  dbConfig >>= \case
    Just conf -> do
      runTestAuto do
        r <- resourceToIO $
          runError @Text $
          mapError @QueryError @Text show $
          mapError @DbError @Text show $
          mapError @(StoreError DbError) @Text show $
          runRandomIO $
          failToEmbed $
          interpretDbConnection conf $
          thunk
        Hedgehog.evalEither r
    Nothing ->
      unit
