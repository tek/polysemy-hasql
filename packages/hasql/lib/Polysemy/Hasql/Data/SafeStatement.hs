module Polysemy.Hasql.Data.SafeStatement where

import qualified Hasql.Statement as Hasql
import Sqel (ResultShape, Statement)
import Sqel.Class.HasqlStatement (hasqlStatement)

newtype SafeStatement query result =
  UnsafeStatement (Hasql.Statement query result)

pattern SafeStatement :: Hasql.Statement query result -> SafeStatement query result
pattern SafeStatement s <- UnsafeStatement s

{-# complete SafeStatement #-}

unsafeStatement ::
  ResultShape a result =>
  Bool ->
  Statement tables query a ->
  SafeStatement query result
unsafeStatement prep statement =
  UnsafeStatement (hasqlStatement prep statement)

safeStatement ::
  ResultShape a result =>
  Bool ->
  Statement '[] query a ->
  SafeStatement query result
safeStatement =
  unsafeStatement
