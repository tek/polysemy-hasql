module Polysemy.Hasql.Test.Tree.Data.DatS where

import Polysemy.Db.Data.Rep (Auto, Prim)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Data.Effect (Adt)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta(ConMeta), TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtSum))

data DatS =
  DatS1 { id :: Int, double1 :: Double }
  |
  DatS2 { id :: Int, text2 :: Text }
  deriving (Eq, Show, Generic)

type DatSSum =
  [
    [Int, Double],
    [Int, Text]
  ]

type DatSAdtMetas =
  '[
    'ConMeta 0 ('NamedField "DatS1") '[
      'TreeMeta ('NamedField "id") Auto Int,
      'TreeMeta ('NamedField "double1") Auto Double
    ],
    'ConMeta 1 ('NamedField "DatS2") '[
      'TreeMeta ('NamedField "id") Auto Int,
      'TreeMeta ('NamedField "text2") Auto Text
    ]
  ]

type DatSAdtMeta =
  'AdtSum DatSAdtMetas

type DatSTreeEffs =
  '[Adt DatSAdtMeta Auto]

type DatSCons =
  '[
    'Kind.Con 0 ('NamedField "DatS1") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
      ],
    'Kind.Con 1 ('NamedField "DatS2") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "text2") '[Prim] ('Kind.Prim Text)
    ]
  ]

type DataSDataTree1 =
  'Kind.Tree ('NamedField "DatS1") '[] (
    'Kind.Prod () '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
    ]
  )

type DatSDataTrees =
  '[
    'Kind.Con 0 ('NamedField "DatS1") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
    ],
    'Kind.Con 1 ('NamedField "DatS2") '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "text2") '[Prim] ('Kind.Prim Text)
    ]
  ]

type DatSPartialNode =
  'Kind.SumProd DatS DatSCons

type DatSNode =
  'Kind.SumProd DatS DatSCons

type DatSDataNode =
  'Kind.Sum DatS DatSDataTrees

type DatSPartialTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSPartialNode

type DatSTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSNode

type DatSDataTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSDataNode
