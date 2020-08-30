module Polysemy.Hasql.Data.StoreTable where

import Hasql.Statement (Statement)

data StoreTable dIn dOut qIn qOut e :: Effect where
  QueryOne :: qOut -> Statement qIn (Maybe qOut) -> StoreTable dIn dOut qIn qOut e m (Either e (Maybe d))
  QueryMany :: qOut -> Statement qIn [qOut] -> StoreTable dIn dOut qIn qOut e m (Either e [d])
  Write :: qOut -> Statement qIn dIn -> StoreTable dIn dOut qIn qOut e m (Either e dOut)

makeSem ''StoreTable
