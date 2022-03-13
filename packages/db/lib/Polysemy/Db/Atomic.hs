module Polysemy.Db.Atomic where

import Control.Concurrent.STM.TVar (newTVarIO)

interpretAtomic ::
  ∀ a r .
  Member (Embed IO) r =>
  a ->
  InterpreterFor (AtomicState a) r
interpretAtomic initial sem = do
  tv <- embed (newTVarIO initial)
  runAtomicStateTVar tv sem
