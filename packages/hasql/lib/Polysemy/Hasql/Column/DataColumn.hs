module Polysemy.Hasql.Column.DataColumn where

import Generics.SOP (All, HCollapse(hcollapse), K(K), NP, hcmap)
import Polysemy.Db.Data.FieldId (fieldIdText)

import Polysemy.Db.Data.FieldId (FieldIdText)
import Polysemy.Db.Text.DbIdentifier (dbIdentifierT, quotedDbId)
import Polysemy.Hasql.Column.Class (TableColumn, tableColumn)
import Polysemy.Hasql.Column.Data.Effect (ContainsFlatten)
import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (Name(Name), Selector(Selector))
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind
import qualified Polysemy.Hasql.Type.Data.DbType as Type

data ColumnPrefix =
  InitPrefix
  |
  TablePrefix
  |
  BasePrefix Text
  |
  TypePrefix Text
  deriving (Eq, Show)

addPrefix ::
  Text ->
  ColumnPrefix ->
  ColumnPrefix
addPrefix segment = \case
  InitPrefix -> TablePrefix
  TablePrefix -> BasePrefix (quotedDbId segment)
  BasePrefix name -> TypePrefix [qt|(#{name}).#{quotedDbId segment}|]
  TypePrefix prefix -> TypePrefix [qt|#{prefix}.#{quotedDbId segment}|]

prefixed :: Text -> ColumnPrefix -> Text
prefixed name = \case
  InitPrefix -> quotedDbId name
  TablePrefix -> quotedDbId name
  BasePrefix prefix -> [qt|(#{prefix}).#{quotedDbId name}|]
  TypePrefix prefix -> [qt|#{prefix}.#{quotedDbId name}|]

class MapColumns (cols :: [Kind.Column]) (cls :: Kind.Column -> Constraint) where
  mapColumns :: ∀ a . (∀ x . cls x => Type.Column x -> a) -> NP Type.Column cols -> [a]

instance (
    All cls cols
  ) => MapColumns cols cls where
    mapColumns f cols =
      hcollapse (hcmap (Proxy @cls) (K . f) cols)

class DataProduct (flatten :: Bool) (c :: Kind.Column) where
  dataProduct :: ColumnPrefix -> Type.Column c -> [Data.Column]

instance (
    MapColumns cols DataProductOrFlatten
  ) => DataProduct 'True ('Kind.Column name effs ('Kind.Prod d cols)) where
  dataProduct prefix (Type.Column _ _ (Type.Prod cols)) =
    join (mapColumns @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols)

instance (
    DataColumn ('Kind.Column name effs tpe)
  ) => DataProduct 'False ('Kind.Column name effs tpe) where
  dataProduct =
    pure .: dataColumn

class DataProductOrFlatten (c :: Kind.Column) where
  dataProductOrFlatten :: ColumnPrefix -> Type.Column c -> [Data.Column]

instance (
    c ~ 'Kind.Column name effs tpe,
    flatten ~ ContainsFlatten effs,
    DataProduct flatten c
  ) => DataProductOrFlatten c where
    dataProductOrFlatten =
      dataProduct @flatten @c

class DataDbType (t :: Kind.DbType) where
  dataDbType :: ColumnPrefix -> Type.DbType t -> Data.DbType

instance (
    MapColumns cols DataProductOrFlatten
  ) => DataDbType ('Kind.Prod d cols) where
  dataDbType prefix (Type.Prod cols) =
    Data.Prod (join (mapColumns @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols))

instance (
    MapColumns cols DataColumn
  ) => DataDbType ('Kind.Sum d cols) where
  dataDbType prefix (Type.Sum cols) =
    Data.Sum (mapColumns @cols @DataColumn (dataColumn prefix) cols)

instance DataDbType ('Kind.Prim d) where
    dataDbType _ Type.Prim =
      Data.Prim

class DataColumn (c :: Kind.Column) where
  dataColumn :: ColumnPrefix -> Type.Column c -> Data.Column

instance (
    DataDbType t,
    FieldIdText name
  ) => DataColumn ('Kind.Column name effs t) where
    dataColumn prefix (Type.Column tpe options dbType) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) tpe options dataType
      where
        dataType =
          dataDbType (addPrefix name prefix) dbType
        name =
          fieldIdText @name

class DataTable (c :: Kind.Column) where
  dataTable :: Type.Column c -> Data.Column

instance (
    DataColumn c
  ) => DataTable c where
  dataTable =
    dataColumn @c InitPrefix

class TableStructure (rep :: *) (d :: *) where
  tableStructure :: Data.Column

instance (
    TableColumn rep d c,
    DataTable c
  ) => TableStructure rep d where
    tableStructure =
      dataTable (tableColumn @rep @d)
