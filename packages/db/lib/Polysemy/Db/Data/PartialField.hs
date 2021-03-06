module Polysemy.Db.Data.PartialField where

import Data.Aeson (Object, Value (Null))
import Data.Aeson.Types (Parser, Value (Object))
import qualified Data.HashMap.Strict as HashMap

import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.Tree (Root (..))
import Polysemy.Db.Tree.Api (TreePrim (..))
import Polysemy.Db.Tree.Data.Params (Params (Params))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.Fold (FoldTree, FoldTreeConcat (..), FoldTreePrim (..), foldTree)
import Polysemy.Db.Tree.Unfold (UnfoldRoot (..), UnfoldTreeExtract (..), UnfoldTreePrim (..))
import qualified Polysemy.Db.Type.Data.Tree as Type

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

data FieldUpdate (path :: FieldPath) (a :: Type) =
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

class Partially d tree | d -> tree where
  partially :: PartialTree tree

instance (
    Root PartialParams Auto d tree
  ) => Partially d tree where
  partially =
    root @PartialParams @Auto @d Keep

type UidPartially i d tree =
  Partially (Uid i d) tree

instance (
    ToJSON d
  ) => FoldTreePrim root () PartialField Object name effs d where
    foldTreePrim = \case
      Keep ->
        mempty
      Update key value ->
        HashMap.singleton key (toJSON value)

filterNulls :: [Object] -> Object
filterNulls =
  mconcat . filter (not . HashMap.null)

instance (
    KnownSymbol name
  ) => FoldTreeConcat 'False () PartialField Object ('NamedField name) effs where
  foldTreeConcat = \case
    [] -> mempty
    os -> HashMap.singleton (symbolText @name) (Object (filterNulls os))

instance FoldTreeConcat 'False () PartialField Object ('NumberedField name num) effs where
  foldTreeConcat =
    filterNulls

instance FoldTreeConcat 'True () PartialField Object ('NamedField name) effs where
  foldTreeConcat =
    filterNulls

instance (
    FoldTree 'True () PartialField Object tree
  ) => ToJSON (Type.Tree () PartialField tree) where
    toJSON =
      Object . foldTree @'True @_ @_ @Object

instance (
    KnownSymbol name,
    FromJSON d
  ) => UnfoldTreePrim () PartialField Parser Value ('NamedField name) effs d where
  unfoldTreePrim = \case
    Object o ->
      maybe (pure Keep) (fmap (Update name) . parseJSON) (HashMap.lookup name o)
    Null ->
      pure Keep
    value ->
      fail [text|invalid json type for partially update of field '#{name}': #{value}|]
    where
      name =
        symbolText @name

instance (
    KnownSymbol name
  ) => UnfoldTreeExtract () PartialField Value ('NamedField name) effs where
  unfoldTreeExtract = \case
    Object o -> fromMaybe Null (HashMap.lookup (symbolText @name) o)
    _ -> Null

instance UnfoldTreeExtract () PartialField Value ('NumberedField name num) effs where
  unfoldTreeExtract =
    id

instance (
    tree ~ 'Kind.Tree name effs ('Kind.Prod d trees),
    Partially d tree,
    UnfoldRoot () PartialField Parser Value tree
  ) => FromJSON (Type.Tree () PartialField ('Kind.Tree name effs ('Kind.Prod d trees))) where
  parseJSON value =
    unfoldRoot value (partially @d)

instance (
    tree ~ 'Kind.Tree name effs ('Kind.SumProd d trees),
    Partially d tree,
    UnfoldRoot () PartialField Parser Value tree
  ) => FromJSON (Type.Tree () PartialField ('Kind.Tree name effs ('Kind.SumProd d trees))) where
  parseJSON value =
    unfoldRoot value (partially @d)
