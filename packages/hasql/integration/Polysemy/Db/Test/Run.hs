module Polysemy.Db.Test.Run where

import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Hedgehog (TestT)

import Polysemy.Db.Data.DbConnection (DbConnection)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.DbConnection (interpretDbConnection)
import Polysemy.Db.Random (Random, runRandomIO)
import Polysemy.Db.Test.DbConfig (dbConfig)
import Polysemy.Fail (Fail, failToEmbed)
import Polysemy.Resource (Resource, resourceToIOFinal)

type TestEffects =
  [
    DbConnection Connection,
    Fail,
    Random,
    Error DbError,
    Error QueryError,
    Error Text,
    Resource,
    Embed IO,
    Final IO
  ]

integrationTest ::
  Sem TestEffects a ->
  TestT IO (Either Text a)
integrationTest thunk = do
  conf <- dbConfig
  lift $
    runFinal $
    embedToFinal $
    resourceToIOFinal $
    runError $
    mapError @QueryError @Text show $
    mapError @DbError @Text show $
    runRandomIO $
    failToEmbed $
    interpretDbConnection conf $
    thunk

