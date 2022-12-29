module Sqel.Column where

import qualified Hasql.Decoders as Hasql
import Hasql.Decoders (Row, custom)

import Sqel.Class.Mods (AddMod (addMod), MapMod, amendMod)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), Struct (Prim))
import Sqel.Data.Mods (Nullable (Nullable), PgDefault (PgDefault), PrimaryKey (PrimaryKey), SetTableName (SetTableName))
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql)
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetName)

ignoreDecoder :: Row (Maybe a)
ignoreDecoder =
  join <$> Hasql.column (Hasql.nullable (custom \ _ _ -> pure Nothing))

pk ::
  AddMod PrimaryKey s0 s1 =>
  Dd s0 ->
  Dd s1
pk =
  addMod PrimaryKey

class MkNullable s0 s1 | s0 -> s1 where
  mkNullable :: Dd s0 -> Dd s1

instance MkNullable ('DdK sel p a 'Prim) ('DdK sel p (Maybe a) 'Prim) where
  mkNullable (Dd sel p s) = Dd sel p s

nullable ::
  ∀ s0 s1 s2 .
  AddMod Nullable s0 s1 =>
  MkNullable s1 s2 =>
  Dd s0 ->
  Dd s2
nullable =
  mkNullable .
  addMod Nullable

nullableAs ::
  ∀ name s0 s1 s2 .
  AddMod Nullable s0 s1 =>
  MkNullable s1 s2 =>
  Rename s2 (SetName s2 name) =>
  Dd s0 ->
  Dd (SetName s2 name)
nullableAs =
  rename . nullable

tableName ::
  ∀ s0 s1 .
  MapMod SetTableName s0 s1 =>
  PgTableName ->
  Dd s0 ->
  Dd s1
tableName name =
  amendMod (SetTableName name)

pgDefault ::
  AddMod PgDefault s0 s1 =>
  Sql ->
  Dd s0 ->
  Dd s1
pgDefault v =
  addMod (PgDefault v)

class Unnamed s0 s1 where

unnamed :: Dd s0 -> Dd s1
unnamed =
  undefined
