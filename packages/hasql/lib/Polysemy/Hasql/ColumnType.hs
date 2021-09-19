{-# language CPP #-}

module Polysemy.Hasql.ColumnType where

import qualified Chronos as Chronos
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.Vector (Vector)
import Fcf (type (@@))
import Path (Path)
import Polysemy.Db.Data.FieldId (FieldId, FieldIdSymbol)
import Polysemy.Db.Data.Rep (Enum, Json, JsonB, Prim)
import Polysemy.Db.SOP.Constraint (DataName)
import Polysemy.Db.Text.DbIdentifier (dbSymbol)
import Polysemy.Db.Tree.Data.Effect (Adt, CustomType, Newtype, Tycon)
import Prelude hiding (Enum)
import Type.Errors (ErrorMessage (ShowType), TypeError)
import Type.Errors.Pretty (type (%), type (<>))

class ColumnType (d :: Type) where
  columnType :: Text
  type ColumnTypeDefined d :: Bool
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
  type ColumnTypeDefined _ = 'True
#else
  type ColumnTypeDefined d = 'True
#endif

instance ColumnType Bool where columnType = "bool"
instance ColumnType Int where columnType = "bigint"
instance ColumnType Int64 where columnType = "bigint"
instance ColumnType Double where columnType = "double precision"
instance ColumnType Text where columnType = "text"
instance ColumnType ByteString where columnType = "bytes"
instance ColumnType UUID where columnType = "uuid"
instance ColumnType Day where columnType = "date"
instance ColumnType LocalTime where columnType = "timestamp without time zone"
instance ColumnType UTCTime where columnType = "timestamp with time zone"
instance ColumnType TimeOfDay where columnType = "time without time zone"
instance ColumnType (TimeOfDay, TimeZone) where columnType = "time with time zone"
instance ColumnType DiffTime where columnType = "interval"
instance ColumnType Chronos.Date where columnType = "date"
instance ColumnType Chronos.Time where columnType = "bigint"
instance ColumnType Chronos.Datetime where columnType = "timestamp without time zone"
instance ColumnType (Path b t) where columnType = "text"
instance ColumnType () where columnType = "unit"

type family NoColumnType (d :: Type) :: ErrorMessage where
  NoColumnType d =
    "Cannot use '" <> 'ShowType d <> "' as a database column." %
    "If this type should be used as a primitive column, implement:" % "" %
    "  instance ColumnType (" <> 'ShowType d <> ") where" % "    columnType = \"postgrestype\"" % "" %
    "If it is supposed to be used as structural type, it is probably lacking an instance of 'Generic'."

class ColumnTypeOrError (defined :: Bool) (d :: Type) where
  columnTypeOrError :: Text

instance ColumnType d => ColumnTypeOrError 'True d where
  columnTypeOrError =
    columnType @d

instance {-# incoherent #-} (
    TypeError (NoColumnType d)
  ) => ColumnTypeOrError defined d where
    columnTypeOrError =
      "error"

class CheckedColumnType (d :: Type) where
  checkedColumnType :: Text

instance ColumnTypeOrError (ColumnTypeDefined d) d => CheckedColumnType d where
  checkedColumnType =
    columnTypeOrError @(ColumnTypeDefined d) @d

class EffectfulColumnType (field :: FieldId) (effs :: [Type]) (d :: Type) where
  effectfulColumnType :: Text

instance (
    TypeError (
    "Cannot use '" <> 'ShowType d <> "' for the database column '" <> (FieldIdSymbol @@ field) <> "'." %
    "Most likely it was not derived via `Column', as it has neither `Prim', " <>
    "`Enum', 'Json' nor `ADT' in its effect stack."
    )
  ) => EffectfulColumnType field '[] d where
    effectfulColumnType =
      "error"

instance {-# overlappable #-} (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (eff : effs) (d :: Type) where
    effectfulColumnType =
      effectfulColumnType @field @effs @d

instance (
    KnownSymbol name,
    DataName d name
  ) => EffectfulColumnType field (Adt meta rep : effs) d where
    effectfulColumnType =
      dbSymbol @name

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Newtype nt d : effs) nt where
    effectfulColumnType =
      effectfulColumnType @field @effs @d

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Tycon [] d : effs) [d] where
    effectfulColumnType =
      effectfulColumnType @field @effs @d <> "[]"

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Tycon Vector d : effs) (Vector d) where
    effectfulColumnType =
      effectfulColumnType @field @effs @d <> "[]"

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Tycon NonEmpty d : effs) (NonEmpty d) where
    effectfulColumnType =
      effectfulColumnType @field @effs @d <> "[]"

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Tycon Set d : effs) (Set d) where
    effectfulColumnType =
      effectfulColumnType @field @effs @d <> "[]"

instance (
    EffectfulColumnType field effs d
  ) => EffectfulColumnType field (Tycon Maybe d : effs) (Maybe d) where
    effectfulColumnType =
      effectfulColumnType @field @effs @d

instance (
    KnownSymbol tpe
  ) => EffectfulColumnType field (CustomType tpe : effs) d where
    effectfulColumnType =
      dbSymbol @tpe

instance (
    CheckedColumnType d
  ) => EffectfulColumnType field '[Prim] d where
    effectfulColumnType =
      checkedColumnType @d

instance EffectfulColumnType field '[Enum] d where
    effectfulColumnType =
      "text"

instance EffectfulColumnType field '[Json] d where
    effectfulColumnType =
      "json"

instance EffectfulColumnType field '[JsonB] d where
    effectfulColumnType =
      "jsonb"
