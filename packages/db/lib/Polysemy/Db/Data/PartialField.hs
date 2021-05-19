module Polysemy.Db.Data.PartialField where

import Data.Aeson (Object, Value (Null))
import Data.Aeson.Types (Value(Object), Parser)
import qualified Data.HashMap.Strict as HashMap

import Polysemy.Db.Data.Column (Auto)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Root(..))
import Polysemy.Db.Tree.Api (TreePrim(..))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreeConcat(..), FoldTreePrim(..), foldTree)
import Polysemy.Db.Tree.Unfold (UnfoldTreeExtract(..), UnfoldTreePrim(..), UnfoldRoot(..))
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))

data PartialField (a :: Type) =
  Update Text a
  |
  Keep
  deriving (Eq, Show, Functor)

instance Applicative PartialField where
  pure = Update "pure"
  Update _ f <*> Update n a = Update n (f a)
  _ <*> _ = Keep

instance Semigroup (PartialField a) where
  _ <> r = r

instance Monoid (PartialField a) where
  mempty = Keep

instance Alternative PartialField where
  empty = Keep
  _ <|> Update t a = Update t a
  l <|> Keep = l

data FieldPath =
  FieldPath [Symbol]
  |
  FieldName Symbol

data FieldUpdate (path :: FieldPath) (a :: *) =
  FieldUpdate a
  deriving (Eq, Show)

data PartialTag =
  PartialTag
  deriving (Eq, Show)

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField
type PartialCon = Type.Con () PartialField
type PartialParams = 'Params PartialTag () PartialField 'True

instance TreePrim PartialTag PartialField name d where
  treePrim _ =
    Keep

instance TreeEffects DefaultEffects rep d effs => TreeEffects PartialTag rep d effs where

class Partial d tree | d -> tree where
  partial :: PartialTree tree

instance (
    Root Auto PartialParams d tree
  ) => Partial d tree where
  partial =
    root @Auto @PartialParams @d Keep

instance (
    ToJSON d
  ) => FoldTreePrim () PartialField Object name effs d where
    foldTreePrim = \case
      Keep ->
        mempty
      Update key value ->
        HashMap.singleton key (toJSON value)

instance FoldTreeConcat () PartialField Object name effs where
  foldTreeConcat = \case
    [] -> mempty
    os -> mconcat (filter (not . HashMap.null) os)

instance (
    FoldTree () PartialField Object tree
  ) => ToJSON (Type.Tree () PartialField tree) where
    toJSON =
      Object . foldTree @_ @_ @Object

instance (
    KnownSymbol name,
    FromJSON d
  ) => UnfoldTreePrim () PartialField Parser Value ('Kind.Tree ('NamedField name) effs ('Kind.Prim d)) where
  unfoldTreePrim = \case
    Object o ->
      maybe (pure Keep) (fmap (Update name) . parseJSON) (HashMap.lookup name o)
    Null ->
      pure Keep
    value ->
      fail [text|invalid json type for partial update of field '#{name}': #{value}|]
    where
      name =
        symbolText @name

instance (
    KnownSymbol name
  ) => UnfoldTreeExtract () PartialField Value ('Kind.Tree ('NamedField name) effs ('Kind.Prod d trees)) where
  unfoldTreeExtract = \case
    Object o -> fromMaybe Null (HashMap.lookup (symbolText @name) o)
    _ -> Null

instance (
    tree ~ 'Kind.Tree name effs ('Kind.Prod d trees),
    Partial d tree,
    UnfoldRoot () PartialField Parser Value tree
  ) => FromJSON (Type.Tree () PartialField ('Kind.Tree name effs ('Kind.Prod d trees))) where
  parseJSON value =
    unfoldRoot value (partial @d)

instance (
    tree ~ 'Kind.Tree name effs ('Kind.SumProd d trees),
    Partial d tree,
    UnfoldRoot () PartialField Parser Value tree
  ) => FromJSON (Type.Tree () PartialField ('Kind.Tree name effs ('Kind.SumProd d trees))) where
  parseJSON value =
    unfoldRoot value (partial @d)
