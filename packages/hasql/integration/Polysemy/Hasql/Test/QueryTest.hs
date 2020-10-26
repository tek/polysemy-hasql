module Polysemy.Hasql.Test.QueryTest where

import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Hasql.Encoders (Params)

import Polysemy.Db.Data.Column (Auto, Flatten, NewtypePrim, PKRep, Prim, PrimaryKey, Sum)
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
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
import Polysemy.Hasql.Table.Query.Where (queryWhere', queryWhereFields', queryWhereProd)
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
     xxor :: XXor
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    content :: NewtypePrim Auto,
    number :: Prim Auto,
    xxor :: Sum XorRep
  }
  deriving (Eq, Show, Generic)

data ContentNumber =
  ContentNumber {
    content :: Content,
    number :: Maybe (LessOrEq Int),
    xxor :: XXor
  }
  deriving (Eq, Show, Generic)

queryWhereProd_ContentNumber_Dat_1 ::
  [Int -> Text]
queryWhereProd_ContentNumber_Dat_1 =
  queryWhereProd @'[Flatten (ProdColumn [NewtypePrim Auto, Prim Auto, Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]])])] @'[ '(Dat, 'FieldInfo "_payload")] @['(Content, 'FieldInfo "content"), '(Maybe (LessOrEq Int), 'FieldInfo "number")]

queryWhereProd_ContentNumber_Dat ::
  [Int -> Text]
queryWhereProd_ContentNumber_Dat =
  queryWhereProd @([Prim PrimaryKey, Flatten (ProdColumn [NewtypePrim Auto, Prim Auto, Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]])])]) @['(UUID, 'FieldInfo "_id"), '(Dat, 'FieldInfo "_payload")] @['(Content, 'FieldInfo "content"), '(Maybe (LessOrEq Int), 'FieldInfo "number")]

queryWhereFields_ContentNumber_Dat ::
  [Int -> Text]
queryWhereFields_ContentNumber_Dat =
  queryWhereFields' @(ProdColumn [Prim PrimaryKey, Flatten (ProdColumn [NewtypePrim Auto, Prim Auto, Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]])])]) @['(UUID, 'FieldInfo "_id"), '(Dat, 'FieldInfo "_payload")] @['(Content, 'FieldInfo "content"), '(Maybe (LessOrEq Int), 'FieldInfo "number")]

queryWhere_ContentNumber_Dat ::
  ReifyRepTable (PKRep Prim UUID DatRep) (Uuid Dat) ~ ProdColumn [Prim PrimaryKey, Flatten (ProdColumn [NewtypePrim Auto, Prim Auto, Sum (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]])])] =>
  QueryWhere (Uuid Dat) ContentNumber
queryWhere_ContentNumber_Dat =
  queryWhere' @(PKRep Prim UUID DatRep) @(Uuid Dat) @ContentNumber

test_queryWhere_derivation :: UnitTest
test_queryWhere_derivation =
  void $ pure queryWhere_ContentNumber_Dat

queryParamsContentNumber :: Params ContentNumber
queryParamsContentNumber =
  queryParams @(Rep ContentNumber) @ContentNumber

target :: Uuid Dat
target =
  Uid (Uid.uuid 2) (Dat "hello" 5 (Lef 8))

prog ::
  Members [Error (StoreError DbError), StoreQuery ContentNumber DbError [Uuid Dat]] r =>
  Members [UuidStore DbError Dat, StoreQuery ContentNumber DbError (Maybe (Uuid Dat))] r =>
  Sem r (Int, Maybe (Uuid Dat))
prog = do
  Store.insert (Uid (Uid.uuid 1) (Dat "hello" 10 (Lef 8)))
  Store.insert target
  Store.insert (Uid (Uid.uuid 3) (Dat "goodbye" 1 (Lef 8)))
  Store.insert (Uid (Uid.uuid 4) (Dat "goodbye" 5 (Lef 8)))
  Store.insert (Uid (Uid.uuid 5) (Dat "hello" 7 (Lef 9)))
  Store.insert (Uid (Uid.uuid 6) (Dat "hello" 7 (Righ 8)))
  r1 :: [Uuid Dat] <- StoreQuery.basicQuery (ContentNumber "hello" Nothing (Lef 8))
  r2 <- StoreQuery.basicQuery (ContentNumber "hello" (Just 6) (Lef 8))
  pure (length r1, r2)

test_query :: UnitTest
test_query = do
  integrationTest do
    withTestStoreTableUidGen @DatRep @Prim \ table@(QueryTable (Table struct _ _) _ _) ->
      interpretDatabase struct $
      interpretOneWith @(PKRep Prim UUID DatRep) (table ^. QueryTable.structure) $
      interpretManyWith @(PKRep Prim UUID DatRep) (table ^. QueryTable.structure) do
        (count, result) <- prog
        count === 2
        assertJust target result
