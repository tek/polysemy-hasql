module Sqel.ColumnConstraints where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Data.Mods (
  Mods (Mods),
  Nullable (Nullable),
  PgDefault (PgDefault),
  PrimaryKey (PrimaryKey),
  Unique (Unique),
  )
import Sqel.Data.Sql (Sql, sql)

data Constraints =
  Constraints {
    unique :: Bool,
    nullable :: Bool,
    extra :: [Sql]
  }
  deriving stock (Eq, Show, Generic)

class ColumnConstraint mod where
  columnConstraint :: mod -> Constraints -> Constraints

instance {-# overlappable #-} ColumnConstraint mod where
  columnConstraint _ = id

instance ColumnConstraint Nullable where
  columnConstraint Nullable =
    #nullable .~ True

instance ColumnConstraint PrimaryKey where
  columnConstraint PrimaryKey Constraints {..} =
    Constraints {
      unique = True,
      extra = "primary key" : extra,
      ..
    }

instance ColumnConstraint PgDefault where
  columnConstraint (PgDefault val) =
    #extra %~ ([sql|default ##{val}|] :)

instance ColumnConstraint Unique where
  columnConstraint Unique Constraints {..} =
    Constraints {
      unique = True,
      extra = "unique" : extra,
      ..
    }

class ColumnConstraints mods where
  collectConstraints :: NP I mods -> Constraints

instance ColumnConstraints '[] where
  collectConstraints Nil = Constraints False False []

instance (
    ColumnConstraint mod,
    ColumnConstraints mods
  ) => ColumnConstraints (mod : mods) where
  collectConstraints (I h :* t) =
    columnConstraint h (collectConstraints t)

columnConstraints ::
  ColumnConstraints mods =>
  Mods mods ->
  (Bool, [Sql])
columnConstraints (Mods mods) =
  (unique, notNull <> extra)
  where
    notNull | nullable = []
            | otherwise = ["not null"]
    Constraints {..} = collectConstraints mods
