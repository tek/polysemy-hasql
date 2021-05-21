module Polysemy.Hasql.Table.ReadNull where

import Hasql.Decoders (Row, column, custom, nullable)

import qualified Polysemy.Db.Kind.Data.Tree as Kind

ignoreDecoder :: Row (Maybe a)
ignoreDecoder =
  join <$> column (nullable (custom \ _ _ -> pure Nothing))

class NullColumns (cs :: [Kind.Tree]) where
  nullColumns :: Row ()

instance NullColumns '[] where
  nullColumns =
    unit

instance (
    NullColumns cs
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prim d) : cs) where
  nullColumns =
    ignoreDecoder *> nullColumns @cs

instance (
    NullColumns c,
    NullColumns cs
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prod d c) : cs) where
  nullColumns =
    nullColumns @c *> nullColumns @cs

class ReadNullCon (c :: Kind.Con) where
  readNullCon :: Row ()

instance {-# overlappable #-} (
    NullColumns ts
  ) => ReadNullCon ('Kind.Con n ts) where
  readNullCon =
    nullColumns @ts

instance (
    NullColumns '[tree]
  ) => ReadNullCon ('Kind.ConUna _n tree) where
  readNullCon =
    nullColumns @'[tree]

class ReadNullCons (cs :: [Kind.Con]) where
  readNullCons :: Row ()

instance ReadNullCons '[] where
  readNullCons =
    unit

instance (
    ReadNullCon c,
    ReadNullCons cs
  ) => ReadNullCons (c : cs) where
  readNullCons =
    readNullCon @c *> readNullCons @cs
