{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.DeriveQuery.UnaSumNumberedTest where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, Product, Sum)
import Polysemy.Db.Data.FieldId (FieldId(NamedField, NumberedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Tree.Table (TableRoot)
import Polysemy.Hasql.Where.Type (MatchTable, MkQueryMeta, QCond(SumPrimCond), ReplicateSum, Segment(FieldSegment))

data Sum1 =
  Sum1 {
    int1 :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data Sum2 =
  Sum2 {
    int2 :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data UnaSum =
  UnaSum1 Sum1
  |
  UnaSum2 Sum2
  deriving (Eq, Show, Generic)

type Sum1Meta =
  ADTMeta' (Product Auto) Sum1

type Sum2Meta =
  ADTMeta' (Product Auto) Sum2

type UnaSumMeta =
  ADTMeta' (Sum Auto) UnaSum

type UnaCons = '[
    'Kind.ConUna ('NamedField "UnaSum1") (
      'Kind.Tree ('NumberedField "UnaSum1" 1) '[ADT Sum1Meta Auto] ('Kind.Prod Sum1 '[
        'Kind.Tree ('NamedField "int1") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    ),
    'Kind.ConUna ('NamedField "UnaSum2") (
      'Kind.Tree ('NumberedField "UnaSum2" 1) '[ADT Sum2Meta Auto] ('Kind.Prod Sum2 '[
        'Kind.Tree ('NamedField "int2") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    )
  ]

type UnaSumType =
  'Kind.Tree ('NamedField "UnaSum") '[ADT UnaSumMeta (Sum Auto)] ('Kind.SumProd UnaSum UnaCons)

derive ::
  dTree ~ UnaSumType =>
  dCons ~ UnaCons =>
  qn ~ 'NamedField "double" =>
  e ~ '[Prim] =>
  t ~ Double =>
  qTree ~ 'Kind.Tree qn e ('Kind.Prim t) =>
  TableRoot (PrimQuery "double") Double qTree =>
  ReplicateSum qTree dCons ~ '[
    'Kind.ConUna ('NamedField "UnaSum1") qTree,
    'Kind.ConUna ('NamedField "UnaSum2") qTree
  ] =>
  fields ~ MatchTable (MkQueryMeta qTree dTree) qTree dTree =>
  fields ~ '[ 'SumPrimCond (Maybe Double) Double '[
    '[
      'FieldSegment ('NamedField "double"),
      'FieldSegment ('NumberedField "UnaSum1" 1)
    ],
    '[
      'FieldSegment ('NamedField "double"),
      'FieldSegment ('NumberedField "UnaSum2" 1)
    ]
  ]] =>
  ()
derive =
  ()

test_deriveQuery_UnaSumNumbered :: UnitTest
test_deriveQuery_UnaSumNumbered =
  pure derive
