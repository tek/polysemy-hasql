module Polysemy.Db.Data.PartialFields where

import Data.Aeson (Object, Value, object, withObject, (.:?))
import Data.Aeson.Types (Parser)
import Generics.SOP (All, AllZipN, Compose, K, K(K), NP, hcmap, hcollapse, hcpure, hsequence', htrans, (:.:)(Comp))
import Generics.SOP.GGP (GCode)

import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Db.SOP.FieldNames (SimpleFieldNames)

-- TODO add FieldUpdates, use only the `a` here, derive the other two at callsite?
data PartialFields (a :: *) (ds :: [*]) (fs :: [Symbol]) =
  PartialFields (NP PartialField ds)

deriving instance All (Eq `Compose` PartialField) ds => Eq (PartialFields a ds fs)
deriving instance All (Show `Compose` PartialField) ds => Show (PartialFields a ds fs)

type family Types' (dss :: [[*]]) :: [*] where
  Types' '[ds] = ds

type family Types (d :: *) :: [*] where
  Types d = Types' (GCode d)

type family PartialFieldsT (d :: *) :: * where
  PartialFieldsT d =
    PartialFields d (Types d) (SimpleFieldNames d)

partialFieldToJson ::
  ToJSON a =>
  PartialField a ->
  Maybe (Text, Value)
partialFieldToJson = \case
  PartialField.Update name value ->
    Just (name, toJSON value)
  PartialField.Keep ->
    Nothing

instance (
    All ToJSON ds
  ) => ToJSON (PartialFields d ds fs) where
  toJSON (PartialFields fs) =
    object (catMaybes (hcollapse (hcmap (Proxy @ToJSON) (K . partialFieldToJson) fs)))

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
    All KnownSymbol fs,
    AllZipN NP ParseField fs ds
  ) => FromJSON (PartialFields d ds (fs :: [Symbol])) where
  parseJSON =
    withObject "PartialFields" (fmap PartialFields . hsequence' . parseFields @ds @fs)
