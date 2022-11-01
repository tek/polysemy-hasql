module Polysemy.Db.Effect.StoreQuery where

-- |'StoreQuery' is a very high level abstraction of a database query.
-- Some parameter structure @q@ goes in, some result @o@ comes out.
-- Those types do not have to correspond to actual database types, the interpreter is responsible for analyzing and
-- synthesizing them.
--
-- @
-- import Polysemy.Db.Data.Cond (GreaterOrEq (GreaterOrEq))
--
-- data User { id :: Int, name :: Text, age :: Int }
-- data OldUser { age :: GreaterOrEq Int }
--
-- prog :: Member (StoreQuery OldUser [User]) r => Sem r [User]
-- prog =
--   StoreQuery.basic (OldUser (GreaterOrEq 80))
-- @
--
-- An interpreter specialized for SQL can then turn @OldUser@ into an appropriate query.
data StoreQuery q o :: Effect where
  Basic :: q -> StoreQuery q o m o

makeSem ''StoreQuery
