module Polysemy.Db.Data.PartialFields where

import Data.Aeson (Object, Value, object, withObject, (.:?))
import Data.Aeson.Types (Parser)
import Generics.SOP (All, AllZipN, Compose, K, K(K), NP, hcmap, hcollapse, hcpure, hsequence', htrans, (:.:)(Comp))

import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.SOP.Constraint (ProductGCode, symbolText)
import Polysemy.Db.SOP.FieldNames (FieldNames)

type FieldTypes d = ProductGCode d

data PartialFields (d :: *) =
  PartialFields (NP PartialField (FieldTypes d))

deriving instance All (Eq `Compose` PartialField) (FieldTypes d) => Eq (PartialFields d)
deriving instance All (Show `Compose` PartialField) (FieldTypes d) => Show (PartialFields d)

encodeField ::
  ToJSON a =>
  PartialField a ->
  Maybe (Text, Value)
encodeField = \case
  PartialField.Update name value ->
    Just (name, toJSON value)
  PartialField.Keep ->
    Nothing

instance (
    All ToJSON (FieldTypes d)
  ) => ToJSON (PartialFields d) where
  toJSON (PartialFields fs) =
    object (catMaybes (hcollapse (hcmap (Proxy @ToJSON) (K . encodeField) fs)))

class ParseField (info :: Symbol) (a :: *) where
  parseField :: Object -> K Text info -> (Parser :.: PartialField) a

instance (
    FromJSON a
  ) => ParseField name a where
  parseField o (K name) =
    Comp $ maybe PartialField.Keep (PartialField.Update name) <$> (o .:? name)

parseFields ::
  ∀ (ds :: [*]) (fs :: [Symbol]) .
  All KnownSymbol fs =>
  AllZipN NP ParseField fs ds =>
  Object ->
  NP (Parser :.: PartialField) ds
parseFields o =
  htrans (Proxy @ParseField) (parseField o) names
  where
    names :: NP (K Text) fs
    names =
      hcpure (Proxy @KnownSymbol) name
    name :: ∀ name . KnownSymbol name => K Text name
    name =
      K (symbolText @name)

instance (
    fs ~ FieldNames d,
    ds ~ FieldTypes d,
    All KnownSymbol fs,
    AllZipN NP ParseField fs ds
  ) => FromJSON (PartialFields d) where
  parseJSON =
    withObject "PartialFields" (fmap PartialFields . hsequence' . parseFields @ds @fs)
