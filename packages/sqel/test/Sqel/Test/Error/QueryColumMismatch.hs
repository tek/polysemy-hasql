{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.QueryColumMismatch where

import Generics.SOP (NP (Nil, (:*)))
import Prelude hiding (sum)

import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Select (SelectFragment)
import Sqel.Data.Uid (Uid)
import Sqel.Prim (prim, primAs)
import Sqel.Product2 (prod, prodAs)
import Sqel.Query (checkQuery)
import Sqel.Uid (uid)

data Pord =
  Pord {
    p1 :: Int,
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    n :: Text,
    pr :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    po :: Pord
  }
  deriving stock (Eq, Show, Generic)

queryColumnMismatch :: [SelectFragment]
queryColumnMismatch =
  frags
  where
    QuerySchema !frags _ = checkQuery qd td :: QuerySchema Q (Uid Int64 Dat)
    qd =
      prod (primAs @"name" :* (prodAs @"pog" (prim :* prim :* Nil)) :* Nil)
    td =
      uid prim (prod (prim :* (prod (prim :* prim :* Nil)) :* Nil))
