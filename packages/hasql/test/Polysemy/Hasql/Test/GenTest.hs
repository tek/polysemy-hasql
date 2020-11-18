module Polysemy.Hasql.Test.GenTest where

-- import Fcf
-- import Generics.SOP.GGP (GCode)
-- import Polysemy.Test (UnitTest)
-- import Type.Errors (IfStuck)

-- data Prim
-- data Gen

-- data Dat =
--   Dat {
--     num :: Int
--   }
--   deriving (Eq, Show, Generic)

-- type family Column a :: * where
--   Column a = IfStuck (GCode a) Prim (Pure Gen)

-- f ::
--   Column Int ~ Prim =>
--   Column Dat ~ Gen =>
--   m ()
-- f =
--   undefined

-- test_gen :: UnitTest
-- test_gen =
--   f
