module Polysemy.Hasql.Test.QueryTest where

import Hasql.Encoders (Params)
import Polysemy.Time (mkDatetime)

import Polysemy.Db.Data.Column (Auto, Flatten, NewtypePrim, Prim, PrimaryKey, Sum, UidRep)
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime(CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.QueryWhere (QueryWhere)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Query.Many (interpretManyWith)
import Polysemy.Hasql.Query.One (interpretOneWith)
import Polysemy.Hasql.Table.Query.Where (queryWhere')
import Polysemy.Hasql.Table.QueryParams (queryParams)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, Rep, SumColumn)
import Polysemy.Hasql.Test.Database (withTestStoreTableUidGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Test (UnitTest, (===))
import Polysemy.Test.Hedgehog (assertJust)

newtype Content =
  Content { unContent :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data XXor =
  Lef { l :: Int }
  |
  Righ { r :: Int }
  deriving (Eq, Show, Generic)

-- xxor.lef.l is null or xxor.lef.l = $4 and xxor.righ.r is null or xxor.righ.r = $5

data XorRep =
  LRep { l :: Prim Auto }
  |
  RRep { r :: Prim Auto }
  deriving (Eq, Show, Generic)

data Dat =
  Dat {
     content :: Content,
     number :: Int,
     xxor :: XXor,
     created :: CreationTime
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    content :: NewtypePrim Auto,
    number :: Prim Auto,
    xxor :: Sum XorRep,
    created :: NewtypePrim Auto
  }
  deriving (Eq, Show, Generic)

data ContentNumber =
  ContentNumber {
    content :: Content,
    number :: Maybe (LessOrEq Int),
    xxor :: XXor
  }
  deriving (Eq, Show, Generic)

type DatRepT =
  '[Flatten (ProdColumn [
    NewtypePrim Auto,
    Prim Auto,
    Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]]),
    NewtypePrim Auto
  ])]

queryWhere_ContentNumber_Dat ::
  ReifyRepTable (UidRep (Prim PrimaryKey) DatRep) (Uuid Dat) ~ ProdColumn [Prim PrimaryKey, Flatten (ProdColumn [NewtypePrim Auto, Prim Auto, Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]]), NewtypePrim Auto])] =>
  QueryWhere (Uuid Dat) ContentNumber
queryWhere_ContentNumber_Dat =
  queryWhere' @(UidRep UUID DatRep) @(Uuid Dat) @ContentNumber

test_queryWhere_derivation :: UnitTest
test_queryWhere_derivation =
  void $ pure queryWhere_ContentNumber_Dat

queryParamsContentNumber :: Params ContentNumber
queryParamsContentNumber =
  queryParams @(Rep ContentNumber) @ContentNumber

creation :: CreationTime
creation =
  CreationTime (mkDatetime 2020 1 1 0 0 0)

target :: Uuid Dat
target =
  Uid (Uid.uuid 2) (Dat "hello" 5 (Lef 8) creation)

prog ::
  Members [Error (StoreError DbError), StoreQuery ContentNumber DbError [Uuid Dat]] r =>
  Members [UuidStore DbError Dat, StoreQuery ContentNumber DbError (Maybe (Uuid Dat))] r =>
  Sem r (Int, Maybe (Uuid Dat))
prog = do
  Store.insert (Uid (Uid.uuid 1) (Dat "hello" 10 (Lef 8) creation))
  Store.insert target
  Store.insert (Uid (Uid.uuid 3) (Dat "goodbye" 1 (Lef 8) creation))
  Store.insert (Uid (Uid.uuid 4) (Dat "goodbye" 5 (Lef 8) creation))
  Store.insert (Uid (Uid.uuid 5) (Dat "hello" 7 (Lef 9) creation))
  Store.insert (Uid (Uid.uuid 6) (Dat "hello" 7 (Righ 8) creation))
  r1 :: [Uuid Dat] <- StoreQuery.basicQuery (ContentNumber "hello" Nothing (Lef 8))
  r2 <- StoreQuery.basicQuery (ContentNumber "hello" (Just 6) (Lef 8))
  pure (length r1, r2)

test_query :: UnitTest
test_query = do
  integrationTest do
    withTestStoreTableUidGen @DatRep @(Prim PrimaryKey) \ table@(QueryTable (Table struct _ _) _ _) ->
      interpretDatabase struct $
      interpretOneWith @(UidRep (Prim PrimaryKey) DatRep) (table ^. QueryTable.structure) $
      interpretManyWith @(UidRep (Prim PrimaryKey) DatRep) (table ^. QueryTable.structure) do
        (count, result) <- prog
        count === 2
        assertJust target result
