{-# language CPP #-}

module Polysemy.Db.Data.PartialField where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Null), (.:?))
import Data.Aeson.Types (Parser, Value (Object))
import Exon (exon)

import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Rep (Auto)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.Tree (Root (..))
import Polysemy.Db.Tree.Api (TreePrim (..))
import Polysemy.Db.Tree.Data.Params (Params (Params))
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.FoldMap (FoldMapTree, FoldMapTreeConcat (..), FoldMapTreePrim (..), foldMapTree)
import Polysemy.Db.Tree.Unfold (UnfoldRoot (..), UnfoldTreeLocal (..), UnfoldTreePrim (..))
import qualified Polysemy.Db.Type.Data.Tree as Type

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HashMap
#endif

data PartialField (a :: Type) =
  Update Text a
  |
  Keep
  deriving stock (Eq, Show, Functor)

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
  deriving stock (Eq, Show)

data PartialTag =
  PartialTag
  deriving stock (Eq, Show)

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
  ) => FoldMapTreePrim root () PartialField Object name effs d where
    foldMapTreePrim = \case
      Keep ->
        mempty
      Update key value ->
        [(fromString (toString key), toJSON value)]

filterNulls :: [Object] -> Object
filterNulls =
  mconcat . filter (not . null)

instance (
    KnownSymbol name
  ) => FoldMapTreeConcat 'False () PartialField Object ('NamedField name) effs where
  foldMapTreeConcat = \case
    [] -> mempty
    os -> [(fromString (symbolVal (Proxy @name)), Object (filterNulls os))]

instance FoldMapTreeConcat 'False () PartialField Object ('NumberedField name num) effs where
  foldMapTreeConcat =
    filterNulls

instance FoldMapTreeConcat 'True () PartialField Object ('NamedField name) effs where
  foldMapTreeConcat =
    filterNulls

instance (
    FoldMapTree 'True () PartialField Object tree
  ) => ToJSON (Type.Tree () PartialField tree) where
    toJSON =
      Object . foldMapTree @'True @_ @_ @Object

instance (
    KnownSymbol name,
    FromJSON d
  ) => UnfoldTreePrim () PartialField Parser Value ('NamedField name) effs d where
  unfoldTreePrim = \case
    Object o ->
      maybe (pure Keep) (fmap (Update name) . parseJSON) =<< (o .:? fromString (toString name))
    Null ->
      pure Keep
    value ->
      fail [exon|invalid json type for partial update of field '#{toString name}': #{show value}|]
    where
      name =
        symbolText @name

instance (
    KnownSymbol name
  ) => UnfoldTreeLocal () PartialField Value ('NamedField name) effs where
  unfoldTreeLocal = \case
#if MIN_VERSION_aeson(2,0,0)
    Object o -> fromMaybe Null (KeyMap.lookup (fromString (symbolVal (Proxy @name))) o)
#else
    Object o -> fromMaybe Null (HashMap.lookup (symbolText @name) o)
#endif
    _ -> Null

instance UnfoldTreeLocal () PartialField Value ('NumberedField name num) effs where
  unfoldTreeLocal =
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
