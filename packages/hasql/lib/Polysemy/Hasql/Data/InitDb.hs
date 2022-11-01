module Polysemy.Hasql.Data.InitDb where

import Hasql.Connection (Connection)

newtype ClientTag =
  ClientTag { unInitTag :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data InitDb m =
  InitDb {
    tag :: ClientTag,
    once :: Bool,
    thunk :: Connection -> m ()
  }
  deriving stock (Generic)

instance Applicative m => Default (InitDb m) where
  def =
    InitDb "global" True (const unit)

hoistInitDb :: (m () -> n ()) -> InitDb m -> InitDb n
hoistInitDb f (InitDb t o th) =
  InitDb t o (f . th)

raiseInitDb :: InitDb (Sem r) -> InitDb (Sem (e : r))
raiseInitDb =
  hoistInitDb raise
