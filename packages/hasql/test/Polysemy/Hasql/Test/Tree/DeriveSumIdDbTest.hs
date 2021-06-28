{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveSumIdDbTest where

import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto, Flatten, Prim, Product, Rep, Sum, UidRep)
import Polysemy.Db.Data.Uid
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (QueryRoot, Root, Tree)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta (ConMeta), TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtProd, AdtSum))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.QueryParams (QueryParams)
import Polysemy.Hasql.QueryRows (QueryRows)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Tree.Table (TableParams, TableRoot)
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
    'ConMeta 0 ('NamedField "SumPKL") '[
      'TreeMeta ('NamedField "l") Prim Int
    ],
    'ConMeta 1 ('NamedField "SumPKR") '[
      'TreeMeta ('NamedField "r") Prim Int
    ]
  ]

type Tr = 'Kind.Tree ('NamedField "dummy") '[Prim] ('Kind.Prim Int)

type SumPKTrees =
  '[
    'Kind.ConUna 0 ('NamedField "SumPKL") ('Kind.Tree ('NamedField "l") '[Prim] ('Kind.Prim Int)),
    'Kind.ConUna 1 ('NamedField "SumPKR") ('Kind.Tree ('NamedField "r") '[Prim] ('Kind.Prim Int))
  ]

type SumPKNode =
  'Kind.SumProd SumPK SumPKTrees

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
  ADT ('AdtProd '[ 'TreeMeta ('NamedField "id") (Sum SumPKRep) SumPK]) (Product (Sum SumPKRep))

type SumPKTree =
  'Kind.Tree ('NamedField "id") '[ADT SumPKMeta (Sum SumPKRep)] SumPKNode

sumIdDerivation ::
  p ~ TableParams =>
  d ~ Uid SumPK SumId =>
  qrep ~ SumPKRep =>
  q ~ SumPK =>
  meta ~ 'TreeMeta ('NamedField "SumId") (Rep '[Product SumIdRecRep]) d =>
  Tree p meta SumIdRecTree =>
  Where Auto SumPKTree q SumIdRecTree d =>
  Tree p ('TreeMeta ('NamedField "id") (Rep '[Sum qrep]) q) SumPKTree =>
  TableRoot SumIdRecRep d SumIdRecTree =>
  QueryRows SumIdRecTree d =>
  QueryParams SumIdRecTree d =>
  BasicSchema Auto d =>
  Tree TableParams ('TreeMeta ('NamedField "SumPK") (Sum qrep) q) qTree =>
  Root TableParams (Sum qrep) q qTree =>
  TableRoot (Sum qrep) q qTree =>
  QueryRoot TableParams (Sum qrep) q d qTree =>
  Schema (Sum qrep) SumIdRecRep q d =>
  ()
sumIdDerivation =
  ()

test_deriveSumIdDb :: UnitTest
test_deriveSumIdDb =
  pure sumIdDerivation
