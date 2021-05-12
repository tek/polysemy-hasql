{-# options_ghc -Wno-all -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumIdDb where

import Polysemy.Db.Data.Column (Auto, Con, Flatten, Prim, Product, Rep, Sum, UidRep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.IdQuery (IdQuery, IdQueryRep, UuidQuery)
import Polysemy.Db.Data.Uid
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (AdtTree, Node, SumConTree, SumIndex, SumNode, SumTrees, Tree)
import Polysemy.Db.Tree.Data (DataParams)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (ADTMeta, AdtMetadata (AdtSum, AdtProd), ConMeta(ConMeta), MaybeADT(MaybeADT), TreeMeta(TreeMeta))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Column.Tree (DbParams, TableColumn)
import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.QueryRows (QueryRows)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)
import Polysemy.Hasql.Table.Table (GenTable)
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
    SumIndex,
    'Kind.Tree ('NamedField "SumPKL") '[Prim] ('Kind.Prim Int),
    'Kind.Tree ('NamedField "SumPKR") '[Prim] ('Kind.Prim Int)
  ]

type SumPKNode =
  'Kind.Prod SumPK SumPKTree

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
  p ~ DbParams =>
  d ~ Uid SumPK SumId =>
  meta ~ 'TreeMeta ('NamedField "SumId") (Rep '[Product SumIdRecRep]) d =>
  Tree p () meta SumIdRecTree =>
  Where IdQueryTree (IdQuery SumPK) SumIdRecTree SumIdRec =>
  Tree p () ('TreeMeta ('NamedField "IdQuery") (Rep '[Product (IdQueryRep (Sum SumPKRep))]) (IdQuery SumPK)) IdQueryTree =>
  TableColumn SumIdRecRep SumIdRec SumIdRecTree =>
  QueryRows SumIdRecTree SumIdRec =>
  QueryParams SumIdRecTree SumIdRec =>
  GenTable Auto SumIdRec =>
  GenQueryTable Auto SumIdRecRep (IdQuery SumPK) SumIdRec =>
  ()
sumIdDerivation =
  ()

test_deriveSumIdDb :: UnitTest
test_deriveSumIdDb =
  pure sumIdDerivation
