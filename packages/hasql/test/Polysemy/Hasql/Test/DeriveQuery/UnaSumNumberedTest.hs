{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.DeriveQuery.UnaSumNumberedTest where

import Fcf (Eval)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Rep (Auto, Prim, PrimQuery, Product, Sum)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta')
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Tree.Table (DbQueryRoot)
import Polysemy.Hasql.Where.Cond (MatchTable, QCond (SumPrimCond))
import Polysemy.Hasql.Where.FlatFields (FieldPath (FieldPath), FlatRoot)
import Polysemy.Hasql.Where.Segment (Segment (ConSegment, FieldSegment, SumIndexSegment))

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
    'Kind.ConUna 0 ('NamedField "UnaSum1") (
      'Kind.Tree ('NumberedField "UnaSum1" 1) '[ADT Sum1Meta Auto] ('Kind.Prod Sum1 '[
        'Kind.Tree ('NamedField "int1") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double") '[Prim] ('Kind.Prim Double)
      ])
    ),
    'Kind.ConUna 1 ('NamedField "UnaSum2") (
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
  e ~ '[PrimQuery "double", Prim] =>
  t ~ Double =>
  qTree ~ 'Kind.Tree qn e ('Kind.Prim t) =>
  DbQueryRoot (PrimQuery "double") Double UnaSum qTree =>
  qFlat ~ Eval (FlatRoot qTree) =>
  qFlat ~ '[ 'FieldPath '[ 'FieldSegment ('NamedField "double") ] Double ] =>
  dFlat ~ Eval (FlatRoot dTree) =>
  dFlat ~ '[
    'FieldPath '[ 'SumIndexSegment UnaSum] Int,
    'FieldPath '[
      'ConSegment 0 ('NamedField "UnaSum1") 'True,
      'FieldSegment ('NumberedField "UnaSum1" 1),
      'FieldSegment ('NamedField "int1")
    ] Int,
    'FieldPath '[
      'ConSegment 0 ('NamedField "UnaSum1") 'True,
      'FieldSegment ('NumberedField "UnaSum1" 1),
      'FieldSegment ('NamedField "double")
    ] Double,
    'FieldPath '[
      'ConSegment 1 ('NamedField "UnaSum2") 'True,
      'FieldSegment ('NumberedField "UnaSum2" 1),
      'FieldSegment ('NamedField "int2")
    ] Int,
    'FieldPath '[
      'ConSegment 1 ('NamedField "UnaSum2") 'True,
      'FieldSegment ('NumberedField "UnaSum2" 1),
      'FieldSegment ('NamedField "double")
    ] Double
  ] =>
  fields ~ MatchTable qTree dTree =>
  fields ~ '[ 'SumPrimCond (Maybe Double) Double '[
    '[
      'ConSegment 0 ('NamedField "UnaSum1") 'True,
      'FieldSegment ('NumberedField "UnaSum1" 1),
      'FieldSegment ('NamedField "double")
    ],
    '[
      'ConSegment 1 ('NamedField "UnaSum2") 'True,
      'FieldSegment ('NumberedField "UnaSum2" 1),
      'FieldSegment ('NamedField "double")
    ]
  ]] =>
  ()
derive =
  ()

test_deriveQuery_UnaSumNumbered :: UnitTest
test_deriveQuery_UnaSumNumbered =
  pure derive
