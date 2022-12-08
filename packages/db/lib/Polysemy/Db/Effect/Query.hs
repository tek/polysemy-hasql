module Polysemy.Db.Effect.Query where

-- |'Query' is a very high level abstraction of a database query.
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
-- prog :: Member (Query OldUser [User]) r => Sem r [User]
-- prog =
--   Query.query (OldUser (GreaterOrEq 80))
-- @
--
-- An interpreter specialized for SQL can then turn @OldUser@ into an appropriate query.
data Query q o :: Effect where
  Query :: q -> Query q o m o

makeSem ''Query
