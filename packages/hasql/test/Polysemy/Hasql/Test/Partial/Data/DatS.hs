module Polysemy.Hasql.Test.Partial.Data.DatS where

import Polysemy.Db.Tree.Meta (ConMeta(ConMeta), TreeMeta(TreeMeta), ADTMetadata (ADTSum))
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Column (Auto, Prim, Con)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree (SumIndexTree)
import qualified Polysemy.Db.Kind.Data.Tree as Kind

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
    'ConMeta ('NamedField "DatS1") '[
      'TreeMeta ('NamedField "id") Auto Int,
      'TreeMeta ('NamedField "double1") Auto Double
    ],
    'ConMeta ('NamedField "DatS2") '[
      'TreeMeta ('NamedField "id") Auto Int,
      'TreeMeta ('NamedField "text2") Auto Text
    ]
  ]

type DatSAdtMeta =
  'ADTSum DatSAdtMetas

type DatSTreeEffs =
  '[ADT DatSAdtMeta Auto]

type DatSTrees =
  '[
    SumIndexTree,
    'Kind.Tree ('NamedField "DatS1") '[] (
      'Kind.Prod (Con ('NamedField "DatS1")) '[
        'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
      ]
    ),
    'Kind.Tree ('NamedField "DatS2") '[] (
      'Kind.Prod (Con ('NamedField "DatS2")) '[
        'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "text2") '[Prim] ('Kind.Prim Text)
      ]
    )
  ]

type DataSDataTree1 =
  'Kind.Tree ('NamedField "DatS1") '[] (
    'Kind.Prod (Con ('NamedField "DatS1")) '[
      'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
      'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
    ]
  )

type DatSDataTrees =
  '[
    'Kind.Tree ('NamedField "DatS1") '[] (
      'Kind.Prod (Con ('NamedField "DatS1")) '[
        'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "double1") '[Prim] ('Kind.Prim Double)
      ]
    ),
    'Kind.Tree ('NamedField "DatS2") '[] (
      'Kind.Prod (Con ('NamedField "DatS2")) '[
        'Kind.Tree ('NamedField "id") '[Prim] ('Kind.Prim Int),
        'Kind.Tree ('NamedField "text2") '[Prim] ('Kind.Prim Text)
      ]
    )
  ]

type DatSPartialNode =
  'Kind.Prod DatS DatSTrees

type DatSNode =
  'Kind.Prod DatS DatSTrees

type DatSDataNode =
  'Kind.Sum DatS DatSDataTrees

type DatSPartialTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSPartialNode

type DatSTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSNode

type DatSDataTree =
  'Kind.Tree ('NamedField "DatS") DatSTreeEffs DatSDataNode
