{-# language CPP #-}

module Polysemy.Hasql.ColumnType where

import qualified Chronos as Chronos
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.Vector (Vector)
import Path (Path)
import Polysemy.Db.Data.Column (Enum, Json, JsonB, Prim)
import Polysemy.Db.SOP.Constraint (DataName)
import Prelude hiding (Enum)
import Type.Errors (ErrorMessage(ShowType), TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Db.Text.DbIdentifier (dbSymbol)
import Polysemy.Db.Tree.Data.Effect (ADT, CustomType, Newtype, Tycon)

class ColumnType (d :: *) where
  columnType :: Text
  type ColumnTypeDefined d :: Bool
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
  type ColumnTypeDefined _ = 'True
#else
  type ColumnTypeDefined d = 'True
#endif

instance ColumnType Bool where columnType = "bool"
instance ColumnType Int where columnType = "bigint"
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

type family NoColumnType (d :: *) :: ErrorMessage where
  NoColumnType d =
    "Cannot use '" <> 'ShowType d <> "' as a database column." %
    "If this type should be used as a primitive column, implement:" % "" %
    "  instance ColumnType (" <> 'ShowType d <> ") where" % "    columnType = \"postgrestype\"" % "" %
    "If it is supposed to be used as structural type, it is probably lacking an instance of 'Generic'."

class ColumnTypeOrError (defined :: Bool) (d :: *) where
  columnTypeOrError :: Text

instance ColumnType d => ColumnTypeOrError 'True d where
  columnTypeOrError =
    columnType @d

instance {-# incoherent #-} (
    TypeError (NoColumnType d)
  ) => ColumnTypeOrError defined d where
    columnTypeOrError =
      "error"

class CheckedColumnType (d :: *) where
  checkedColumnType :: Text

instance ColumnTypeOrError (ColumnTypeDefined d) d => CheckedColumnType d where
  checkedColumnType =
    columnTypeOrError @(ColumnTypeDefined d) @d

class EffectfulColumnType (effs :: [*]) (d :: *) where
  effectfulColumnType :: Text

instance (
    TypeError (
    "Cannot use '" <> 'ShowType d <> "' as a database column." %
    "Most likely it was not derived via `Column', as it has neither `Prim', " <>
    "`Enum', 'Json' nor `ADT' in its effect stack."
    )
  ) => EffectfulColumnType '[] d where
    effectfulColumnType =
      "error"

instance {-# overlappable #-} (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (eff : effs) (d :: *) where
    effectfulColumnType =
      effectfulColumnType @effs @d

instance (
    KnownSymbol name,
    DataName d name
  ) => EffectfulColumnType (ADT meta rep : effs) d where
    effectfulColumnType =
      dbSymbol @name

instance (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (Newtype nt d : effs) nt where
    effectfulColumnType =
      effectfulColumnType @effs @d

instance (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (Tycon [] d : effs) [d] where
    effectfulColumnType =
      effectfulColumnType @effs @d <> "[]"

instance (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (Tycon Vector d : effs) (Vector d) where
    effectfulColumnType =
      effectfulColumnType @effs @d <> "[]"

instance (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (Tycon NonEmpty d : effs) (NonEmpty d) where
    effectfulColumnType =
      effectfulColumnType @effs @d <> "[]"

instance (
    EffectfulColumnType effs d
  ) => EffectfulColumnType (Tycon Maybe d : effs) (Maybe d) where
    effectfulColumnType =
      effectfulColumnType @effs @d

instance (
    KnownSymbol tpe
  ) => EffectfulColumnType (CustomType tpe : effs) d where
    effectfulColumnType =
      dbSymbol @tpe

instance (
    CheckedColumnType d
  ) => EffectfulColumnType '[Prim] d where
    effectfulColumnType =
      checkedColumnType @d

instance EffectfulColumnType '[Enum] d where
    effectfulColumnType =
      "text"

instance EffectfulColumnType '[Json] d where
    effectfulColumnType =
      "json"

instance EffectfulColumnType '[JsonB] d where
    effectfulColumnType =
      "jsonb"
