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
    ReadNullCons c,
    NullColumns cs
  ) => NullColumns ('Kind.Tree n eff ('Kind.Prod d c) : cs) where
  nullColumns =
    readNullCons @c *> nullColumns @cs

-- -- TODO this should be necessary if 'TcEffect Maybe d' is already in the stack, but doesn't appear to be in practice
-- instance (
--     QueryRow eff rep (Maybe d),
--     NullColumns ds cs
--   ) => NullColumns (d : ds) ('Kind.Tree n eff ('Kind.Prim (Maybe d) t s) : cs) where
--   nullColumns =
--     void (queryRow @eff @rep @(Maybe d)) *> nullColumns @ds @cs

class ReadNullCon (c :: Kind.Tree) where
  readNullCon :: Row ()

instance ReadNullCon ('Kind.Tree n eff ('Kind.Prim d)) where
  readNullCon =
    nullColumns @'[ 'Kind.Tree n eff ('Kind.Prim d)]

instance (
    NullColumns cs
  ) => ReadNullCon ('Kind.Tree n eff ('Kind.Prod d cs)) where
  readNullCon =
    nullColumns @cs

-- instance (
--     ProductCoded d dSub,
--     ReadNullCon rSub dSub,
--     ReadNullCon reps ds
--   ) => ReadNullCon (ProdColumn (Flatten rSub : reps)) (d : ds) where
--   readNullCon =
--     readNullCon @rSub @dSub *> readNullCon @reps @ds

class ReadNullCons (cs :: [Kind.Tree]) where
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
