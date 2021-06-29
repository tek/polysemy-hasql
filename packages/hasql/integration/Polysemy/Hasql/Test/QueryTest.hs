{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.QueryTest where

import Data.Time (UTCTime)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Rep (Auto, Flatten, Prim, PrimaryKey, Product, Sum, UidRep)
import Polysemy.Db.Data.Cond (LessOrEq (LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime (CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.InitDbError (InitDbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid), Uuid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, Tycon)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta (ConMeta), TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Meta (ADTMeta', AdtMetadata (AdtProd, AdtSum))
import Polysemy.Log (Log)
import Polysemy.Test (Hedgehog, UnitTest, (===))
import Polysemy.Test.Hedgehog (assertJust)
import Polysemy.Time (GhcTime, mkDatetime)

import Polysemy.Hasql.Table.DataColumn (dataTable)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Query.Many (interpretMany)
import Polysemy.Hasql.Query.One (interpretOne)
import Polysemy.Hasql.QueryParams (QueryParams, queryParams)
import Polysemy.Hasql.Store (interpretStoreDbFullGen)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Tree.Table (TableRoot, tableRoot)
import Polysemy.Hasql.Where (queryWhere)

newtype Content =
  Content { unContent :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data XXor =
  Lef { l :: Double }
  |
  Righ { r :: Bool }
  deriving (Eq, Show, Generic)

data XXorRep =
  LRep { l :: Prim }
  |
  RRep { r :: Prim }
  deriving (Eq, Show, Generic)

data Number =
  Number {
    number :: Int,
    otherNumber :: Int
  }
  deriving (Eq, Show, Generic)

data NumberRep =
  NumberRep {
    number :: Auto,
    otherNumber :: Auto
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
     _content :: Content,
     number :: NumberWrap,
     xxor :: XXor,
     created :: CreationTime
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    content :: Auto,
    number :: Flatten NumberWrapRep,
    xxor :: Sum XXorRep,
    created :: Auto
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

type XXorMeta =
  'AdtSum '[
    'ConMeta 0 ('NamedField "Lef") '[ 'TreeMeta ('NamedField "l") Auto Double],
    'ConMeta 1 ('NamedField "Righ") '[ 'TreeMeta ('NamedField "r") Auto Bool]
  ]

type XXorCols =
  [
    'Kind.ConUna 0 ('NamedField "Lef") ('Kind.Tree ('NamedField "l") '[Prim] ('Kind.Prim Double)),
    'Kind.ConUna 1 ('NamedField "Righ") ('Kind.Tree ('NamedField "r") '[Prim] ('Kind.Prim Bool))
  ]

type XXorType rep =
  'Kind.SumProd XXor XXorCols

type ContentNumberMeta =
  'AdtProd '[
    'TreeMeta ('NamedField "content") Auto Content,
    'TreeMeta ('NamedField "otherNumber") Auto (Maybe Int),
    'TreeMeta ('NamedField "number") Auto (Maybe (LessOrEq Int)),
    'TreeMeta ('NamedField "xxor") Auto XXor
  ]

type XXorCol =
  'Kind.Tree ('NamedField "xxor") '[ADT XXorMeta (Sum XXorRep)] (XXorType (Sum XXorRep))

type XXorColAuto =
  'Kind.Tree ('NamedField "xxor") '[ADT XXorMeta Auto] (XXorType Auto)

type ContentNumberType =
  'Kind.Tree ('NamedField "ContentNumber") '[ADT ContentNumberMeta Auto] (
    'Kind.Prod ContentNumber '[
      'Kind.Tree ('NamedField "content") '[Newtype Content Text, Prim] ('Kind.Prim Content),
      'Kind.Tree ('NamedField "otherNumber") '[Tycon Maybe Int, Prim] ('Kind.Prim (Maybe Int)),
      'Kind.Tree ('NamedField "number") '[Tycon Maybe (LessOrEq Int), Newtype (LessOrEq Int) Int, Prim] ('Kind.Prim (Maybe (LessOrEq Int))),
      XXorColAuto
    ]
  )

type DatType name rep sumRep =
  'Kind.Tree name '[ADT (ADTMeta' rep Dat) rep] ('Kind.Prod Dat '[
    'Kind.Tree ('NamedField "content") '[Newtype Content Text, Prim] ('Kind.Prim Content),
    'Kind.Tree ('NamedField "number") '[ADT (ADTMeta' (Flatten NumberWrapRep) NumberWrap) (Flatten NumberWrapRep)] (
      'Kind.Prod NumberWrap '[
        'Kind.Tree ('NamedField "numberWrap") '[ADT (ADTMeta' (Flatten NumberRep) Number) (Flatten NumberRep)] (
          'Kind.Prod Number '[
            'Kind.Tree ('NamedField "number") '[Prim] ('Kind.Prim Int),
            'Kind.Tree ('NamedField "otherNumber") '[Prim] ('Kind.Prim Int)
          ]
        )
      ]
    ),
    'Kind.Tree ('NamedField "xxor") '[ADT (ADTMeta' sumRep XXor) sumRep] ('Kind.SumProd XXor XXorCols),
    'Kind.Tree ('NamedField "created") '[Newtype CreationTime UTCTime, Prim] ('Kind.Prim CreationTime)
  ])

type DatTable =
  DatType ('NamedField "Dat") (Product DatRep) (Sum XXorRep)

type UidDatType =
  'Kind.Tree ('NamedField "Dat") '[ADT (ADTMeta' (Product (UidRep PrimaryKey DatRep)) (Uuid Dat)) (Product (UidRep PrimaryKey DatRep))] (
    'Kind.Prod (Uuid Dat) [
      'Kind.Tree ('NamedField "id") '[PrimaryKey, Prim] ('Kind.Prim UUID),
      DatType ('NamedField "payload") (Flatten DatRep) (Sum XXorRep)
    ]
  )

queryParams_ContentNumber ::
  QueryParams ContentNumberType ContentNumber =>
  Params ContentNumber
queryParams_ContentNumber =
  queryParams @ContentNumberType @ContentNumber

queryWhere_ContentNumber ::
  TableRoot DatRep Dat DatTable =>
  Where ContentNumber Dat
queryWhere_ContentNumber =
  queryWhere @Auto @ContentNumberType @ContentNumber @DatTable @Dat

queryWhere_ContentNumber_Uid ::
  TableRoot (UidRep PrimaryKey DatRep) (Uuid Dat) (UidDatType) =>
  Where ContentNumber (Uuid Dat)
queryWhere_ContentNumber_Uid =
  queryWhere @Auto @ContentNumberType @ContentNumber @UidDatType @(Uuid Dat)

test_derivation :: IO ()
test_derivation = do
  void (pure (dataTable (tableRoot @(UidRep Auto DatRep) @(Uuid Dat))))
  void (pure queryParams_ContentNumber)
  void (pure queryWhere_ContentNumber)
  void (pure queryWhere_ContentNumber_Uid)

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
  Members [Stop DbError, StoreQuery ContentNumber [Uuid Dat] !! DbError] r =>
  Members [UuidStore Dat !! DbError, StoreQuery ContentNumber (Maybe (Uuid Dat)) !! DbError] r =>
  Sem r (Int, Maybe (Uuid Dat))
prog = do
  restop @DbError @(UuidStore Dat) do
    Store.insert (Uid (Uid.uuid 1) (Dat "hello" (num 10) (Lef 8) creation))
    Store.insert target
    Store.insert (Uid (Uid.uuid 3) (Dat "goodbye" (num 1) (Lef 8) creation))
    Store.insert (Uid (Uid.uuid 4) (Dat "goodbye" (num 5) (Lef 8) creation))
    Store.insert (Uid (Uid.uuid 5) (Dat "hello" (num 7) (Lef 9) creation))
    Store.insert (Uid (Uid.uuid 6) (Dat "hello" (num 7) (Righ True) creation))
    r1 :: [Uuid Dat] <- restop (StoreQuery.basic (ContentNumber "hello" Nothing Nothing (Lef 8)))
    r2 <- restop (StoreQuery.basic (ContentNumber "hello" Nothing (Just 6) (Lef 8)))
    pure (length r1, r2)

prog' ::
  Members [Database !! DbError, Stop DbError, Error InitDbError, GhcTime, Hedgehog IO, Log, Embed IO] r =>
  Sem r ()
prog' =
  interpretQuery @Auto @(UidRep PrimaryKey DatRep) $
  interpretStoreDbFullGen @DatRep $
  interpretOne @ContentNumber @(Uuid Dat) $
  interpretMany @ContentNumber @(Uuid Dat) do
    (count, result) <- prog
    2 === count
    assertJust target result

test_query :: UnitTest
test_query = do
  integrationTest prog'
