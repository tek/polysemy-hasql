module Polysemy.Hasql.Test.Run where

import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Hedgehog (TestT)
import Polysemy.Fail (Fail, failToEmbed)
import Polysemy.Resource (Resource)
import qualified Polysemy.Test as Hedgehog
import Polysemy.Test (Hedgehog, Test, runTestAuto)
import Polysemy.Test.Data.TestError (TestError)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Random (Random, runRandomIO)
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Test.Database (withTestConnection)
import Polysemy.Hasql.Test.DbConfig (dbConfig)

type TestEffects =
  [
    DbConnection Connection,
    Fail,
    Random,
    Error (StoreError DbError),
    Error DbError,
    Error QueryError,
    Error Text,
    Test,
    Error TestError,
    Hedgehog IO,
    Embed IO,
    Resource,
    Final IO
  ]

integrationTest ::
  Sem TestEffects () ->
  TestT IO ()
integrationTest thunk =
  dbConfig >>= \case
    Just conf -> do
      runTestAuto do
        r <- runError @Text $
          mapError @QueryError @Text show $
          mapError @DbError @Text show $
          mapError @(StoreError DbError) @Text show $
          runRandomIO $
          failToEmbed $
          withTestConnection conf $
          thunk
        Hedgehog.evalEither r
    Nothing ->
      unit
