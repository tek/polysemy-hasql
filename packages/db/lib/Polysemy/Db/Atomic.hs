module Polysemy.Db.Atomic where

interpretAtomic ::
  ∀ a r .
  Member (Embed IO) r =>
  a ->
  InterpreterFor (AtomicState a) r
interpretAtomic initial sem = do
  tv <- newTVarIO initial
  runAtomicStateTVar tv sem
