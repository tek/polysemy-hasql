module Sqel.Data.PgType where

import qualified Data.Map.Strict as Map
import qualified Exon
import Lens.Micro.Extras (view)
import Prettyprinter (Pretty (pretty), nest, sep, vsep, (<+>))

import Sqel.Data.Create (Create (Create))
import Sqel.Data.PgTypeName (PgCompName, PgTableName, pattern PgTypeName)
import Sqel.Data.Select (Select (Select))
import Sqel.Data.Selector (Selector (unSelector), assign, nameSelector)
import Sqel.Data.Sql (Sql, ToSql (toSql), sql, sqlQuote)
import Sqel.SOP.Constraint (symbolText)
import Sqel.Sql.CommaSep (CommaSep (CommaSep))
import Sqel.Sql.Delete (Delete (Delete))
import Sqel.Sql.From (From (From))
import Sqel.Sql.Insert (Insert (Insert))
import Sqel.Sql.Into (Into (Into))
import Sqel.Sql.Returning (Returning (Returning))
import Sqel.Sql.Update (Update (Update))
import Sqel.Text.DbIdentifier (dbIdentifierT, dbSymbol)

newtype PgPrimName =
  PgPrimName { unPgPrimName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, Semigroup, Monoid)

json ''PgPrimName

instance Pretty PgPrimName where
  pretty (PgPrimName n) = pretty n

pgPrimName ::
  ∀ name .
  KnownSymbol name =>
  PgPrimName
pgPrimName =
  PgPrimName (dbSymbol @name)

newtype PgProdName =
  PgProdName { unPgProdName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype PgColumnName =
  PgColumnName { unPgColumnName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord)

json ''PgColumnName

instance Pretty PgColumnName where
  pretty (PgColumnName n) = pretty n

instance ToSql PgColumnName where
  toSql =
    sqlQuote . unPgColumnName

pgColumnName ::
  Text ->
  PgColumnName
pgColumnName n =
  PgColumnName (dbIdentifierT n)

instance IsString PgColumnName where
  fromString = pgColumnName . fromString

newtype PgTypeRef =
  PgTypeRef { unPgTypeRef :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''PgTypeRef

instance Pretty PgTypeRef where
  pretty (PgTypeRef n) = pretty n

instance ToSql PgTypeRef where
  toSql = sqlQuote . unPgTypeRef

pgTypeRef ::
  Text ->
  PgTypeRef
pgTypeRef n =
  PgTypeRef (dbIdentifierT n)

pgCompRef :: PgCompName -> PgTypeRef
pgCompRef (PgTypeName n) =
  PgTypeRef n

pgTypeRefSym ::
  ∀ tname .
  KnownSymbol tname =>
  PgTypeRef
pgTypeRefSym =
  pgTypeRef (symbolText @tname)

data ColumnType =
  ColumnPrim { name :: PgPrimName, unique :: Bool, constraints :: [Sql] }
  |
  ColumnComp { pgType :: PgTypeRef, unique :: Bool, constraints :: [Sql] }
  deriving stock (Eq, Show, Generic)

json ''ColumnType

data PgColumn =
  PgColumn {
    name :: PgColumnName,
    pgType :: ColumnType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSql (Create PgColumn) where
  toSql (Create PgColumn {..}) =
    case pgType of
      ColumnPrim (PgPrimName tpe) _ (Exon.intercalate " " -> params) ->
        [sql|##{name} ##{tpe} #{params}|]
      ColumnComp (PgTypeRef tpe) _ (Exon.intercalate " " -> params) ->
        [sql|##{name} ##{tpe} #{params}|]

newtype PgColumns =
  PgColumns { unPgColumns :: [PgColumn] }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

data StructureType =
  StructurePrim { name :: PgPrimName, unique :: Bool, constraints :: [Sql] }
  |
  StructureComp { compName :: PgCompName, struct :: PgStructure, unique :: Bool, constraints :: [Sql] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

structureToColumn :: StructureType -> ColumnType
structureToColumn = \case
  StructurePrim {..} -> ColumnPrim {..}
  StructureComp (PgTypeName ref) _ unique constr -> ColumnComp (PgTypeRef ref) unique constr

instance Pretty PgColumns where
  pretty (PgColumns cs) =
    vsep $ cs <&> \case
      PgColumn n (ColumnPrim t _ opt) -> "*" <+> pretty n <+> pretty t <+> sep (pretty <$> opt)
      PgColumn n (ColumnComp t _ opt) -> "+" <+> pretty n <+> pretty t <+> sep (pretty <$> opt)

instance ToSql (CommaSep PgColumns) where
  toSql (CommaSep (PgColumns cols)) =
    toSql (CommaSep (view #name <$> cols))

instance ToSql (Create PgColumns) where
  toSql (Create (PgColumns cols)) =
    [sql|(##{CommaSep (Create <$> cols)})|]

newtype PgStructure =
  PgStructure { unPgColumns :: [(PgColumnName, StructureType)] }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

structureToColumns :: PgStructure -> PgColumns
structureToColumns (PgStructure cols) =
  PgColumns (uncurry PgColumn . second structureToColumn <$> cols)

data PgComposite =
  PgComposite {
    name :: PgCompName,
    columns :: PgColumns
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty PgComposite where
  pretty PgComposite {..} =
    nest 2 (vsep ["type" <+> pretty name, pretty columns])

newtype TableSelectors =
  TableSelectors { unTableSelectors :: [Selector] }
  deriving stock (Eq, Show, Generic)

instance ToSql (CommaSep TableSelectors) where
  toSql (CommaSep (TableSelectors s)) =
    toSql (CommaSep (unSelector <$> s))

instance ToSql (Select TableSelectors) where
  toSql (Select s) =
    "select " <> toSql (CommaSep s)

newtype TableValues =
  TableValues { unTableValues :: [Sql] }
  deriving stock (Eq, Show, Generic)

data PgTable a =
  PgTable {
    name :: PgTableName,
    columns :: PgColumns,
    types :: Map PgTypeRef PgComposite,
    selectors :: TableSelectors,
    values :: TableValues,
    structure :: PgStructure
  }
  deriving stock (Show, Generic)

instance Pretty (PgTable a) where
  pretty PgTable {..} =
    nest 2 (vsep (("table" <+> pretty name) : pretty columns : (pretty <$> Map.elems types)))

instance ToSql (Create (PgTable a)) where
  toSql (Create PgTable {name, columns}) =
    [sql|create table ##{name} ##{Create columns}|]

instance ToSql (Select (PgTable a)) where
  toSql (Select PgTable {name, selectors}) =
    [sql|##{Select selectors} ##{From name}|]

instance ToSql (Update (PgTable a)) where
  toSql (Update PgTable {columns = PgColumns columns, values = TableValues values}) =
    [sql|update set ##{CommaSep assigns}|]
    where
      assigns = zipWith assign colNames values
      colNames = columns <&> \ (PgColumn (PgColumnName name) _) -> nameSelector name

instance ToSql (Returning (PgTable a)) where
  toSql (Returning (PgTable {selectors})) =
    [sql|returning ##{CommaSep selectors}|]

instance ToSql (Insert (PgTable a)) where
  toSql (Insert PgTable {name, columns, values = TableValues values}) =
    [sql|insert ##{Into name} (##{CommaSep columns}) values (##{CommaSep values})|]

instance ToSql (Delete (PgTable a)) where
  toSql (Delete PgTable {name}) =
    [sql|delete ##{From name}|]
