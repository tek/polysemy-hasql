{-# options_ghc -fconstraint-solver-iterations=10 #-}

module Sqel.Test.Bug where

import Sqel.Comp (CompItem)
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (Dd, DdType, ProductField (ProductField), type (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Merge (merge)
import Sqel.Names (named)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.ReifyCodec (reifyCodec)
import Sqel.Type (Merge, Prim, Prod, type (*>), type (>))

data Wrap a = Wrap { w :: a }
  deriving stock (Eq, Show, Generic)

data Wrap2 a = Wrap2 { id :: Text, w :: a }
  deriving stock (Eq, Show, Generic)

type DdWrap d s = Prod (Wrap d) *> Merge s

type DdWrap2 s = (Prod (Wrap2 (DdType s))) *> Prim "id" Text > Merge s

ddWrap2 ::
  ∀ d s merged .
  merged ~ Merge s =>
  CompItem ('ProductField "w" d) (Dd merged) merged =>
  Dd s ->
  Dd (DdWrap2 (DdWrap d s))
ddWrap2 w =
  prod (prim :> merge (prod (merge w)))

data Bug =
  Bug {
    a :: Int,
    b :: Int,
    c :: Int,
    d :: Int,
    e :: Int
  }
  deriving stock (Eq, Show, Generic)

type DdBug =
  Prod Bug *>
    Prim "a" Int >
    Prim "b" Int >
    Prim "c" Int >
    Prim "d" Int >
    Prim "e" Int

data WrapBug =
  WrapBug { w :: Bug }
  deriving stock (Eq, Show, Generic)

type DdWrapBug =
  Prod WrapBug *> Merge DdBug

type DdWrap2WrapBug =
  DdWrap2 DdWrapBug

ddBug :: Dd DdBug
ddBug =
  prod prims

ddWrapBug :: Dd DdWrapBug
ddWrapBug =
  prod (merge ddBug)

ddUidWrapBug :: Dd DdWrap2WrapBug
ddUidWrapBug =
  prod (prim :> merge ddWrapBug)

ddWrapWrapBug :: Dd (DdWrap2 (DdWrap WrapBug DdWrapBug))
ddWrapWrapBug =
  ddWrap2 ddWrapBug

schemaUidWrapBug :: FullCodec (Wrap2 WrapBug)
schemaUidWrapBug =
  reifyCodec ddUidWrapBug

schemaWrapWrapBug :: FullCodec (Wrap2 (Wrap WrapBug))
schemaWrapWrapBug =
  reifyCodec ddWrapWrapBug

queryUidWrapBug :: QuerySchema Int (Wrap2 WrapBug)
queryUidWrapBug =
  checkQuery (named @"a" prim) ddUidWrapBug
