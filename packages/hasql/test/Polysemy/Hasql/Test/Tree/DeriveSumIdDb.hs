{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumIdDb where

import Polysemy.Db.Data.Column (Auto, Flatten, Prim, Product, Rep, Sum, UidRep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.IdQuery (IdQuery, IdQueryRep)
import Polysemy.Db.Data.Uid
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Tree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta(ConMeta), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtSum, AdtProd))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Tree.Table (TableParams, TableRoot)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.QueryRows (QueryRows)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema)
import Polysemy.Hasql.Where (Where)

data SumPK =
  SumPKL { l :: Int }
  |
  SumPKR { r :: Int }
  deriving (Eq, Show, Generic)

data SumPKRep =
  SumPKLRep { l :: Prim }
  |
  SumPKRRep { r :: Prim }
  deriving (Eq, Show, Generic)

data SumId =
  SumId { number :: Int }
  deriving (Eq, Show, Generic)

data SumIdRep =
  SumIdRep { number :: Prim }
  deriving (Eq, Show, Generic)

data SumPKQ =
  SumPKQ {
    number :: Int
  }
  deriving (Eq, Show, Generic)

type SumIdRecRep =
  UidRep (Sum SumPKRep) SumIdRep

type SumIdRec =
  Uid SumPK SumId

type SumPKMeta =
  'AdtSum '[
    'ConMeta ('NamedField "SumPKL") '[
      'TreeMeta ('NamedField "l") Prim Int
    ],
    'ConMeta ('NamedField "SumPKR") '[
      'TreeMeta ('NamedField "r") Prim Int
    ]
  ]

type Tr = 'Kind.Tree ('NamedField "dummy") '[Prim] ('Kind.Prim Int)

type SumPKTree =
  '[
    'Kind.ConUna ('NamedField "SumPKL") ('Kind.Tree ('NamedField "l") '[Prim] ('Kind.Prim Int)),
    'Kind.ConUna ('NamedField "SumPKR") ('Kind.Tree ('NamedField "r") '[Prim] ('Kind.Prim Int))
  ]

type SumPKNode =
  'Kind.SumProd SumPK SumPKTree

type SumIdMeta =
  'AdtProd '[
    'TreeMeta ('NamedField "number") Prim Int
  ]

type SumIdEffs =
  ADT SumIdMeta (Flatten SumIdRep)

type SumIdTrees =
  '[
    'Kind.Tree ('NamedField "number") '[Prim] ('Kind.Prim Int)
  ]

type SumIdRecEffs =
  '[
    ADT ('AdtProd '[
      'TreeMeta ('NamedField "id") (Sum SumPKRep) SumPK,
      'TreeMeta ('NamedField "payload") (Flatten SumIdRep) SumId
    ]) (Product SumIdRecRep)
  ]

type SumIdRecTree =
  'Kind.Tree ('NamedField "SumId") SumIdRecEffs ('Kind.Prod SumIdRec '[
    'Kind.Tree ('NamedField "id") '[ADT SumPKMeta (Sum SumPKRep)] SumPKNode,
    'Kind.Tree ('NamedField "payload") '[SumIdEffs] ('Kind.Prod SumId SumIdTrees)
  ])

type IdQueryEffs =
  ADT ('AdtProd '[ 'TreeMeta ('NamedField "id") (Sum SumPKRep) SumPK]) (Product (IdQueryRep (Sum SumPKRep)))

type IdQueryTree =
  'Kind.Tree ('NamedField "IdQuery") '[IdQueryEffs] ('Kind.Prod (IdQuery SumPK) '[
    'Kind.Tree ('NamedField "id") '[ADT SumPKMeta (Sum SumPKRep)] SumPKNode
  ])

sumIdDerivation ::
  p ~ TableParams =>
  d ~ Uid SumPK SumId =>
  meta ~ 'TreeMeta ('NamedField "SumId") (Rep '[Product SumIdRecRep]) d =>
  Tree p meta SumIdRecTree =>
  Where Auto IdQueryTree (IdQuery SumPK) SumIdRecTree SumIdRec =>
  Tree p ('TreeMeta ('NamedField "IdQuery") (Rep '[Product (IdQueryRep (Sum SumPKRep))]) (IdQuery SumPK)) IdQueryTree =>
  TableRoot SumIdRecRep SumIdRec SumIdRecTree =>
  QueryRows SumIdRecTree SumIdRec =>
  QueryParams SumIdRecTree SumIdRec =>
  BasicSchema Auto SumIdRec =>
  Schema Auto SumIdRecRep (IdQuery SumPK) SumIdRec =>
  ()
sumIdDerivation =
  ()

test_deriveSumIdDb :: UnitTest
test_deriveSumIdDb =
  pure sumIdDerivation
