module Polysemy.Db.Type.Data.Tree where

import qualified Data.Text as Text
import Generics.SOP (All, Compose, K(K), NP, hcmap, hcollapse)
import qualified Text.Show as Show

import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdTextRaw)
import qualified Polysemy.Db.Kind.Data.Tree as Kind

-- TODO generalize Prod/Sum or make the index column first class
data Node (t :: Type) (n :: Type -> Type) :: Kind.Node -> Type where
  Prim :: n d -> Node t n ('Kind.Prim d)
  Prod :: NP (Tree t n) sub -> Node t n ('Kind.Prod d sub)
  Sum :: NP (Tree t n) sub -> Node t n ('Kind.Sum d sub)

instance Show (n d) => Show (Node t n ('Kind.Prim d)) where
  show (Prim n) =
    "Prim (" <> show n <> ")"

instance (
    All (Compose Show (Tree t n)) sub
  ) => Show (Node t n ('Kind.Prod d sub)) where
  show (Prod sub) =
    [qt|Prod [#{Text.intercalate ", " (hcollapse (hcmap (Proxy @(Compose Show (Tree t n))) (K . show @Text) sub))}]|]

instance (
    All (Compose Show (Tree t n)) sub
  ) => Show (Node t n ('Kind.Sum d sub)) where
  show (Sum sub) =
    [qt|Prod [#{Text.intercalate ", " (hcollapse (hcmap (Proxy @(Compose Show (Tree t n))) (K . show @Text) sub))}]|]

data Tree (t :: Type) (n :: Type -> Type) :: Kind.Tree -> Type where
  Tree :: t -> Node t n node -> Tree t n ('Kind.Tree name eff node)

data ColumnData =
  ColumnData {
    name :: Text,
    options :: ColumnOptions
  }
  deriving (Eq, Show)

type DbType = Node ColumnData Proxy
type Column = Tree ColumnData Proxy

instance (Eq (Node t n ('Kind.Prim d))) where
  _ == _ =
    True

instance (
  All (Compose Eq (Tree t n)) sub
  ) => (Eq (Node t n ('Kind.Prod d sub))) where
  Prod l == Prod r =
    l == r

instance (
  All (Compose Eq (Tree t n)) sub
  ) => (Eq (Node t n ('Kind.Sum d sub))) where
  Sum l == Sum r =
    l == r

instance (
    Show t,
    FieldIdText name,
    Show (Node t n node)
  ) => Show (Tree t n ('Kind.Tree name eff node)) where
  show (Tree t dbType) =
    [qt|Tree "#{fieldIdTextRaw @name}" #{t} (#{dbType})|]

deriving instance (
    Eq t,
    Eq (Node t n node)
  ) => Eq (Tree t n ('Kind.Tree name eff node))
