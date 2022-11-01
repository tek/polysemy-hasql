module Sqel.Data.PgTypeName where

import Exon (exon)
import Prettyprinter (Pretty (pretty))
import Text.Show (showParen, showsPrec)

import Sqel.Data.Sql (ToSql (toSql), sql, sqlQuote)
import Sqel.SOP.Constraint (symbolText)
import Sqel.Sql.From (From (From))
import Sqel.Sql.Into (Into (Into))
import Sqel.Text.DbIdentifier (dbIdentifierT)

type PgTypeName :: Bool -> Type
data PgTypeName table where
  UnsafePgTableName :: Text -> PgTypeName 'True
  UnsafePgCompName :: Text -> PgTypeName 'False

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

pgTableName ::
  Text ->
  PgTypeName 'True
pgTableName =
  UnsafePgTableName . dbIdentifierT

pgCompName ::
  Text ->
  PgTypeName 'False
pgCompName name =
  UnsafePgCompName [exon|ph_type__#{dbIdentifierT name}|]

instance IsString (PgTypeName 'True) where
  fromString =
    pgTableName . fromString

instance IsString (PgTypeName 'False) where
  fromString =
    pgCompName . fromString

type MkPgTypeName :: Symbol -> Bool -> Constraint
class MkPgTypeName name table where
  pgTypeName :: PgTypeName table

instance (
    KnownSymbol name
  ) => MkPgTypeName name 'True where
    pgTypeName = pgTableName (symbolText @name)

instance (
    KnownSymbol name
  ) => MkPgTypeName name 'False where
    pgTypeName = pgCompName (symbolText @name)
