module Polysemy.Hasql.Data.ConnectionTag where

import Exon (ToSegment (toSegment))

data ConnectionTag =
  GlobalTag
  |
  NamedTag Text
  |
  SerialTag Integer
  deriving stock (Eq, Show, Ord, Generic)

instance IsString ConnectionTag where
  fromString =
    NamedTag . fromString

instance (
    IsString a
  ) => ToSegment ConnectionTag a where
    toSegment = \case
      GlobalTag -> "global"
      NamedTag n -> fromText n
      SerialTag n -> show n
