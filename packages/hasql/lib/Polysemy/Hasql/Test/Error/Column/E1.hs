{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Polysemy.Hasql.Test.Error.Column.E1 where

-- import Generics.SOP (NP)
-- import Polysemy.Db.Data.Rep (Auto, Prim)

-- import Polysemy.Db.Tree (columns)
-- import Polysemy.Hasql.Type.Data.DbType (DbType, PrimType, ProdType)

-- data Sub =
--   Sub {
--     int :: Int
--   }
--   deriving (Eq, Show, Generic)

-- data SubRep =
--   SubRep {
--     int :: Prim Auto
--   }
--   deriving (Eq, Show, Generic)

-- data Dat =
--   Dat {
--     sub :: Sub
--   }
--   deriving (Eq, Show, Generic)

-- data DatRep =
--   DatRep {
--     sub :: SubRep
--   }
--   deriving (Eq, Show, Generic)

-- errorColumnE1 :: NP DbType '[ProdType SubRep Sub '[PrimType (Prim Auto) Int]]
-- errorColumnE1 =
--   columns @DatRep @Dat
