module Polysemy.Db.Type.Data.Tree where

import qualified Data.Text as Text
import Exon (exon)
import Generics.SOP (All, Compose, K (K), NP, NS, hcmap, hcollapse)
import Prelude hiding (Compose)
import qualified Text.Show as Show

import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdTextRaw)
import qualified Polysemy.Db.Kind.Data.Tree as Kind

data Con (t :: Type) (n :: Type -> Type) :: Kind.Con -> Type where
  Con :: NP (Tree t n) sub -> Con t n ('Kind.Con num name sub)
  ConUna :: Tree t n tree -> Con t n ('Kind.ConUna num name tree)

instance (
    KnownNat num,
    All (Compose Show (Tree t n)) sub
  ) => Show (Con t n ('Kind.Con num name sub)) where
  show (Con sub) =
    toString [exon|Con #{show (natVal (Proxy @num))} [#{Text.intercalate ", " trees}]|]
    where
      trees =
        hcollapse (hcmap (Proxy @(Compose Show (Tree t n))) (K . show @Text) sub)

instance (
    KnownNat num,
    Show (Tree t n tree)
  ) => Show (Con t n ('Kind.ConUna num name tree)) where
  show (ConUna sub) =
    [exon|Con (#{show sub})|]

instance (
    Eq (Tree t n tree)
  ) => (Eq (Con t n ('Kind.ConUna num name tree))) where
  ConUna l == ConUna r =
    l == r

instance (
    All (Compose Eq (Tree t n)) sub
  ) => (Eq (Con t n ('Kind.Con num name sub))) where
  Con l == Con r =
    l == r

data Node (t :: Type) (n :: Type -> Type) :: Kind.Node -> Type where
  Prim :: n d -> Node t n ('Kind.Prim d)
  Prod :: n d -> NP (Tree t n) sub -> Node t n ('Kind.Prod d sub)
  Sum :: n d -> NS (Con t n) cons -> Node t n ('Kind.Sum d cons)
  SumProd :: n d -> NP (Con t n) cons -> Node t n ('Kind.SumProd d cons)

instance Show (n d) => Show (Node t n ('Kind.Prim d)) where
  show (Prim n) =
    "Prim (" <> show n <> ")"

instance (
    Show (n d),
    All (Compose Show (Tree t n)) sub
  ) => Show (Node t n ('Kind.Prod d sub)) where
  show (Prod n sub) =
    [exon|Prod #{show @String n} [#{subS}]|]
    where
      subS =
        intercalate ", " (hcollapse (hcmap (Proxy @(Compose Show (Tree t n))) (K . show @String) sub))

instance (
    Show (n d),
    All (Compose Show (Con t n)) cons
  ) => Show (Node t n ('Kind.Sum d cons)) where
  show (Sum n sub) =
    [exon|Sum #{show @String n} [#{subS}]|]
    where
      subS =
        hcollapse (hcmap (Proxy @(Compose Show (Con t n))) (K . show @String) sub)

instance (
    Show (n d),
    All (Compose Show (Con t n)) sub
  ) => Show (Node t n ('Kind.SumProd d sub)) where
  show (SumProd n sub) =
    [exon|SumProd #{show @String n} [#{subS}]|]
    where
      subS =
        intercalate ", " (hcollapse (hcmap (Proxy @(Compose Show (Con t n))) (K . show @String) sub))

instance Eq (n d) => Eq (Node t n ('Kind.Prim d)) where
  Prim l == Prim r =
    l == r

instance (
    Eq (n d),
    All (Compose Eq (Tree t n)) sub
  ) => (Eq (Node t n ('Kind.Prod d sub))) where
  Prod nl l == Prod nr r =
    nl == nr && l == r

instance (
    Eq (n d),
    All (Compose Eq (Con t n)) cons
  ) => (Eq (Node t n ('Kind.Sum d cons))) where
  Sum nl l == Sum nr r =
    nl == nr && l == r

instance (
    Eq (n d),
    All (Compose Eq (Con t n)) cons
  ) => (Eq (Node t n ('Kind.SumProd d cons))) where
  SumProd nl l == SumProd nr r =
    nl == nr && l == r

data Tree (t :: Type) (n :: Type -> Type) :: Kind.Tree -> Type where
  Tree :: t -> Node t n node -> Tree t n ('Kind.Tree name eff node)

instance (
    Show t,
    FieldIdText name,
    Show (Node t n node)
  ) => Show (Tree t n ('Kind.Tree name eff node)) where
  show (Tree t dbType) =
    toString [exon|Tree "#{fieldIdTextRaw @name}" #{show t} (#{show dbType})|]

deriving stock instance (
    Eq t,
    Eq (Node t n node)
  ) => Eq (Tree t n ('Kind.Tree name eff node))
