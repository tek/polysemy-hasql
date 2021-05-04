{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.QueryTest where

import Data.Time (UTCTime)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Auto, Flatten, Prim, PrimaryKey, Product, Sum, UidRep)
import Polysemy.Db.Data.Cond (LessOrEq(LessOrEq))
import Polysemy.Db.Data.CreationTime (CreationTime(CreationTime))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.InitDbError (InitDbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import qualified Polysemy.Db.Data.StoreQuery as StoreQuery
import Polysemy.Db.Data.StoreQuery (StoreQuery)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (SumIndexTree)
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype, Tycon)
import Polysemy.Db.Tree.Meta (ADTMeta', ADTMetadata(ADTSum, ADTProd), ConMeta(ConMeta), TreeMeta(TreeMeta))
import Polysemy.Log (Log)
import Polysemy.Test (Hedgehog, UnitTest, (===))
import Polysemy.Test.Hedgehog (assertJust)
import Polysemy.Time (GhcTime, mkDatetime)

import Polysemy.Hasql.Column.DataColumn (dataTable)
import Polysemy.Hasql.Column.Effect (ResolveColumnEffects)
import Polysemy.Hasql.Column.Tree (TableColumn, tableColumn)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Query.Many (interpretMany)
import Polysemy.Hasql.Query.One (interpretOne)
import Polysemy.Hasql.QueryParams (QueryParams, queryParams)
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Test.Run (integrationTest)
import qualified Polysemy.Hasql.Type.Data.DbType as Type
import Polysemy.Hasql.Where (QCond(SimpleCond), Segment(SumSegment, FieldSegment), queryWhere)

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
  'ADTSum '[
    'ConMeta ('NamedField "Lef") '[ 'TreeMeta ('NamedField "l") Auto Double],
    'ConMeta ('NamedField "Righ") '[ 'TreeMeta ('NamedField "r") Auto Bool]
    ]

type LefCol =
  'Kind.Tree ('NamedField "Lef") '[Prim] ('Kind.Prim Double)

type XXorCols =
  [
    SumIndexTree,
    LefCol,
    'Kind.Tree ('NamedField "Righ") '[Prim] ('Kind.Prim Bool)
  ]

type XXorType rep =
  'Kind.Sum XXor XXorCols

type ContentNumberMeta =
  'ADTProd '[
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
    'Kind.Tree ('NamedField "xxor") '[ADT (ADTMeta' sumRep XXor) sumRep] ('Kind.Sum XXor XXorCols),
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

column_ContentNumber ::
  ResolveColumnEffects Auto ContentNumber '[ADT ContentNumberMeta Auto] =>
  Type.Column ContentNumberType
column_ContentNumber =
  tableColumn @Auto @ContentNumber

queryParams_ContentNumber ::
  QueryParams ContentNumberType ContentNumber =>
  Params ContentNumber
queryParams_ContentNumber =
  queryParams @ContentNumberType @ContentNumber

type LefQuery =
  '[ 'SimpleCond Double Double ['FieldSegment ('NamedField "Lef"), 'SumSegment ('NamedField "")] ]

type CNColsQuery =
  '[
    'SimpleCond Int Int ['FieldSegment ('NamedField "sum_index"), 'SumSegment ('NamedField "")],
    'SimpleCond Double Double ['FieldSegment ('NamedField "Lef"), 'SumSegment ('NamedField "")],
    'SimpleCond Double Double ['FieldSegment ('NamedField "Righ"), 'SumSegment ('NamedField "")]
    ]

type CNQuery =
  '[
    'SimpleCond Int Int ['FieldSegment ('NamedField "sum_index"), 'SumSegment ('NamedField "xxor")],
    'SimpleCond Double Double ['FieldSegment ('NamedField "Lef"), 'SumSegment ('NamedField "xxor")],
    'SimpleCond Double Double ['FieldSegment ('NamedField "Righ"), 'SumSegment ('NamedField "xxor")]
    ]

queryWhere_ContentNumber ::
  TableColumn DatRep Dat DatTable =>
  Where Dat ContentNumber
queryWhere_ContentNumber =
  queryWhere @ContentNumberType @ContentNumber @DatTable @Dat

queryWhere_ContentNumber_Uid ::
  TableColumn (UidRep PrimaryKey DatRep) (Uuid Dat) (UidDatType) =>
  Where (Uuid Dat) ContentNumber
queryWhere_ContentNumber_Uid =
  queryWhere @ContentNumberType @ContentNumber @UidDatType @(Uuid Dat)

test_derivation :: IO ()
test_derivation = do
  void (pure (dataTable (tableColumn @(UidRep Auto DatRep) @(Uuid Dat))))
  void (pure column_ContentNumber)
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
  interpretStoreDbFullGenUid @DatRep @PrimaryKey $
  interpretOne @ContentNumber @(Uuid Dat) $
  interpretMany @ContentNumber @(Uuid Dat) do
    (count, result) <- prog
    2 === count
    assertJust target result

test_query :: UnitTest
test_query = do
  integrationTest prog'
