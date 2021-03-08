module Polysemy.Hasql.Data.Database where

import Hasql.Connection (Connection)
import Hasql.Statement (Statement)
import Polysemy.Time (TimeUnit)

import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.DeriveStatement (DeriveQuery)

data InitDb m =
  InitDb {
    tag :: Text,
    thunk :: Connection -> m ()
  }

hoistInitDb :: (m () -> n ()) -> InitDb m -> InitDb n
hoistInitDb f (InitDb t th) =
  InitDb t (f . th)

instance Applicative m => Default (InitDb m) where
  def =
    InitDb "global" (const unit)

raiseInitDb :: InitDb (Sem r) -> InitDb (Sem (e : r))
raiseInitDb (InitDb t th) =
  InitDb t (raise . th)

-- |This effect provides the capability to execute statements, either as hasql's 'Statement' or raw SQL.
-- Additionally, it exposes managed access to the raw 'Connection' resource and automatic table initialization as
-- higher-order effects.
--
-- With the minimal stack, an SQL query can be executed in two fashions.
-- One is to use automatically derived codecs:
--
-- @
-- prog :: Member Database r => Sem r ()
-- prog = do
--   user :: Maybe User <- Database.sql () "select * from users where id = 1"
--   user :: [User] <- Database.sql "guest" "select * from users where name = $1"
-- @
--
-- The other works by providing an explicit 'Statement':
--
-- @
-- statement :: Statement Text User
-- statement = ...
--
-- prog :: Member Database r => Sem r ()
-- prog = do
--   user <- Database.runStatement "guest" statement
-- @
--
-- For documentation on the individual constructors, see the module page.
data Database :: Effect where
  Info :: Database m (Text, Int)
  WithInit :: InitDb m -> m a -> Database m a
  Connect :: (Connection -> m a) -> Database m a
  RunStatement :: p -> Statement p o -> Database m o
  RunStatementRetrying :: TimeUnit t => t -> p -> Statement p o -> Database m o
  Sql :: DeriveQuery p r => p -> SqlCode -> Database m r

makeSem ''Database
