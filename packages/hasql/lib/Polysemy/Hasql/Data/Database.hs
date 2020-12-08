module Polysemy.Hasql.Data.Database where

import Hasql.Connection (Connection)
import Hasql.Statement (Statement)

import Polysemy.Time (TimeUnit)

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

data Database :: Effect where
  Info :: Database m (Text, Int)
  WithInit :: InitDb m -> m a -> Database m a
  Connect :: (Connection -> m a) -> Database m a
  RunStatement :: q -> Statement q o -> Database m o
  RunStatementRetrying :: TimeUnit t => t -> q -> Statement q o -> Database m o

makeSem ''Database
