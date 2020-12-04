module Polysemy.Hasql.Test.QueryTest where

import Hasql.Encoders (Params)
import Polysemy.Resume (Stop, restop, type (!))
import Polysemy.Test (UnitTest, (===))
import Polysemy.Test.Hedgehog (assertJust)
import Polysemy.Time (mkDatetime)

import Polysemy.Db.Data.Column (Auto, Flatten, NewtypePrim, Prim, PrimaryKey, Sum, UidRep)
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime(CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryWhere (QueryWhere)
import Polysemy.Hasql.Query.Many (interpretManyWith)
import Polysemy.Hasql.Query.One (interpretOneWith)
import Polysemy.Hasql.Table.Query.Where (queryWhere)
import Polysemy.Hasql.Table.QueryParams (queryParams)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, Rep, SumColumn)
import Polysemy.Hasql.Test.Database (withTestStoreTableUidGen)
import Polysemy.Hasql.Test.Run (integrationTest)

newtype Content =
  Content { unContent :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data XXor =
  Lef { l :: Int }
  |
  Righ { r :: Int }
  deriving (Eq, Show, Generic)

data XorRep =
  LRep { l :: Prim Auto }
  |
  RRep { r :: Prim Auto }
  deriving (Eq, Show, Generic)

data Number =
  Number {
    number :: Int,
    otherNumber :: Int
  }
  deriving (Eq, Show, Generic)

data NumberRep =
  NumberRep {
    number :: Prim Auto,
    otherNumber :: Prim Auto
  }
  deriving (Eq, Show, Generic)

data NumberWrap =
  NumberWrap {
    numberWrap :: Number
  }
  deriving (Eq, Show, Generic)

data NumberWrapRep =
  NumberWrapRep {
    numberWrap :: Flatten NumberRep
  }
  deriving (Eq, Show, Generic)

data Dat =
  Dat {
     content :: Content,
     number :: NumberWrap,
     xxor :: XXor,
     created :: CreationTime
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    content :: NewtypePrim Auto,
    number :: Flatten NumberWrapRep,
    xxor :: Sum Auto XorRep,
    created :: NewtypePrim Auto
  }
  deriving (Eq, Show, Generic)

data ContentNumber =
  ContentNumber {
    content :: Content,
    otherNumber :: Maybe Int,
    number :: Maybe (LessOrEq Int),
    xxor :: XXor
  }
  deriving (Eq, Show, Generic)

type DatRepT =
  '[Flatten (ProdColumn [
    NewtypePrim Auto,
    Prim Auto,
    Sum Auto (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]]),
    NewtypePrim Auto
  ])]

queryWhere_ContentNumber_Dat ::
  ReifyRepTable (UidRep (Prim PrimaryKey) DatRep) (Uuid Dat) ~ ProdColumn [Prim PrimaryKey, Flatten (ProdColumn [NewtypePrim Auto, Flatten (ProdColumn '[Flatten (ProdColumn '[Prim Auto, Prim Auto])]), Sum Auto (SumColumn '[ProdColumn '[Prim Auto], ProdColumn '[Prim Auto]]), NewtypePrim Auto])] =>
  QueryWhere (Uuid Dat) ContentNumber
queryWhere_ContentNumber_Dat =
  queryWhere @(UidRep UUID DatRep) @(Uuid Dat) @ContentNumber

test_queryWhere_derivation :: UnitTest
test_queryWhere_derivation =
  void $ pure queryWhere_ContentNumber_Dat

queryParamsContentNumber :: Params ContentNumber
queryParamsContentNumber =
  queryParams @(Rep ContentNumber) @ContentNumber

creation :: CreationTime
creation =
  CreationTime (mkDatetime 2020 1 1 0 0 0)

num :: Int -> NumberWrap
num n =
  NumberWrap (Number n 555)

target :: Uuid Dat
target =
  Uid (Uid.uuid 2) (Dat "hello" (num 5) (Lef 8) creation)

prog ::
  Members [Stop DbError, StoreQuery ContentNumber DbError [Uuid Dat] ! DbError] r =>
  Members [UuidStore Dat ! DbError, StoreQuery ContentNumber DbError (Maybe (Uuid Dat)) ! DbError] r =>
  Sem r (Int, Maybe (Uuid Dat))
prog = do
  restop @DbError @(UuidStore Dat) do
    Store.insert (Uid (Uid.uuid 1) (Dat "hello" (num 10) (Lef 8) creation))
    Store.insert target
    Store.insert (Uid (Uid.uuid 3) (Dat "goodbye" (num 1) (Lef 8) creation))
    Store.insert (Uid (Uid.uuid 4) (Dat "goodbye" (num 5) (Lef 8) creation))
    Store.insert (Uid (Uid.uuid 5) (Dat "hello" (num 7) (Lef 9) creation))
    Store.insert (Uid (Uid.uuid 6) (Dat "hello" (num 7) (Righ 8) creation))
    r1 :: [Uuid Dat] <- restop (StoreQuery.basic (ContentNumber "hello" Nothing Nothing (Lef 8)))
    r2 <- restop (StoreQuery.basic (ContentNumber "hello" Nothing (Just 6) (Lef 8)))
    pure (length r1, r2)

test_query :: UnitTest
test_query = do
  integrationTest do
    withTestStoreTableUidGen @DatRep @(Prim PrimaryKey) \ table ->
      interpretOneWith @(UidRep (Prim PrimaryKey) DatRep) (table ^. QueryTable.structure) $
      interpretManyWith @(UidRep (Prim PrimaryKey) DatRep) (table ^. QueryTable.structure) do
        (count, result) <- restop @DbError @(UuidStore Dat) prog
        count === 2
        assertJust target result
