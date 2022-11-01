module Polysemy.Hasql.Queue.Data.Queue where

import Prelude hiding (Queue)

type family InputConn (queue :: Symbol) :: Symbol where
  InputConn queue =
    AppendSymbol queue "-input"

type family OutputConn (queue :: Symbol) :: Symbol where
  OutputConn queue =
    AppendSymbol queue "-output"

type family Queue (queue :: Symbol) t :: Constraint where
  Queue queue t =
    (
      Ord t,
      KnownSymbol queue,
      KnownSymbol (InputConn queue),
      KnownSymbol (OutputConn queue)
    )

type family QueueInput (queue :: Symbol) t :: Constraint where
  QueueInput queue t =
    (Ord t, KnownSymbol (InputConn queue))

type family QueueOutput (queue :: Symbol) t :: Constraint where
  QueueOutput queue t =
    (Ord t, KnownSymbol (OutputConn queue))

newtype QueueName =
  QueueName { unQueueName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)
