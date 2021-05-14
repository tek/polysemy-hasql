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
    -- maybeEff ~ 'TcEffect Maybe d,
    -- QueryRow (maybeEff : eff) rep (Maybe d),
    NullColumns c,
    NullColumns cs
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prod d c) : cs) where
  nullColumns =
    nullColumns @c *> nullColumns @cs

-- -- TODO this should be necessary if 'TcEffect Maybe d' is already in the stack, but doesn't appear to be in practice
-- instance (
--     QueryRow eff rep (Maybe d),
--     NullColumns ds cs
--   ) => NullColumns (d : ds) ('Kind.Tree n eff ('Kind.Prim (Maybe d) t s) : cs) where
--   nullColumns =
--     void (queryRow @eff @rep @(Maybe d)) *> nullColumns @ds @cs

class ReadNullCon (c :: Kind.Con) where
  readNullCon :: Row ()

instance {-# overlappable #-} (
    NullColumns ts
  ) => ReadNullCon ('Kind.Con n ts) where
  readNullCon =
    nullColumns @ts

instance ReadNullCon ('Kind.ConUna _n ('Kind.Tree n eff ('Kind.Prim d))) where
  readNullCon =
    void ignoreDecoder

-- instance (
--     ProductCoded d dSub,
--     ReadNullCon rSub dSub,
--     ReadNullCon reps ds
--   ) => ReadNullCon (ProdColumn (Flatten rSub : reps)) (d : ds) where
--   readNullCon =
--     readNullCon @rSub @dSub *> readNullCon @reps @ds

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
