module Sqel.Data.PgTypeName where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.GADT.Show (GShow (gshowsPrec))
import Exon (exon)
import Prettyprinter (Pretty (pretty))

import Sqel.Data.Sel (SelPrefix (DefaultPrefix), TypeName)
import Sqel.Data.Sql (ToSql (toSql), sql, sqlQuote)
import Sqel.Data.SqlFragment (From (From), Into (Into))
import Sqel.SOP.Constraint (symbolText)
import Sqel.Text.DbIdentifier (dbIdentifierT)

type PgTypeName :: Bool -> Type
data PgTypeName table where
  UnsafePgTableName :: Text -> PgTypeName 'True
  UnsafePgCompName :: Text -> PgTypeName 'False

instance GShow PgTypeName where gshowsPrec = showsPrec

type PgTableName =
  PgTypeName 'True

type PgCompName =
  PgTypeName 'False

unsafePgTypeName :: PgTypeName table -> Text
unsafePgTypeName = \case
  UnsafePgTableName n -> n
  UnsafePgCompName n -> n

pattern PgTypeName :: Text -> PgTypeName table
pattern PgTypeName name <- (unsafePgTypeName -> name)
{-# complete PgTypeName #-}

pattern PgTableName :: Text -> PgTypeName table
pattern PgTableName name <- (UnsafePgTableName name)

pattern PgCompName :: Text -> PgTypeName table
pattern PgCompName name <- (UnsafePgCompName name)

{-# complete PgTableName, PgCompName #-}

instance Eq (PgTypeName table) where
  UnsafePgTableName l == UnsafePgTableName r = l == r
  UnsafePgCompName l == UnsafePgCompName r = l == r

instance Show (PgTypeName table) where
  showsPrec d =
    showParen (d > 10) . \case
      UnsafePgTableName n -> [exon|UnsafePgTableName #{showsPrec 11 n}|]
      UnsafePgCompName n -> [exon|UnsafePgCompName #{showsPrec 11 n}|]

instance Pretty (PgTypeName table) where
  pretty (UnsafePgCompName n) = pretty n
  pretty (UnsafePgTableName n) = pretty n

instance ToSql (PgTypeName table) where
  toSql (PgTypeName n) =
    sqlQuote n

instance ToSql (From PgTableName) where
  toSql (From n) =
    [sql|from ##{n}|]

instance ToSql (Into PgTableName) where
  toSql (Into n) =
    [sql|into ##{n}|]

instance FromJSON PgTableName where
  parseJSON v = UnsafePgTableName <$> parseJSON v

instance FromJSON PgCompName where
  parseJSON v = UnsafePgCompName <$> parseJSON v

instance ToJSON (PgTypeName t) where
  toJSON = toJSON . unsafePgTypeName

pgTableName ::
  Text ->
  PgTypeName 'True
pgTableName =
  UnsafePgTableName . dbIdentifierT

pgCompName ::
  Text ->
  PgTypeName 'False
pgCompName name =
  UnsafePgCompName (dbIdentifierT name)

instance IsString PgTableName where
  fromString =
    pgTableName . fromString

instance IsString PgCompName where
  fromString =
    pgCompName . fromString

instance Ord (PgTypeName table) where
  compare = comparing unsafePgTypeName

type MkPgTypeName :: SelPrefix -> Symbol -> Bool -> Symbol -> Constraint
class KnownSymbol tname => MkPgTypeName prefix name table tname | prefix name table -> tname where
  pgTypeName :: PgTypeName table

instance (
    KnownSymbol name
  ) => MkPgTypeName 'DefaultPrefix name 'True name where
    pgTypeName = pgTableName (symbolText @name)

instance (
    TypeName prefix name tname
  ) => MkPgTypeName prefix name 'False tname where
    pgTypeName = pgCompName (symbolText @tname)
