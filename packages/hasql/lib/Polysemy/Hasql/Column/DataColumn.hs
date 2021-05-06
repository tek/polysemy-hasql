module Polysemy.Hasql.Column.DataColumn where

import Generics.SOP (All, AllN, CollapseTo, HAp, HCollapse(hcollapse), K(K), NP, NS, Prod, hcmap)
import Generics.SOP.Constraint (SListIN)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Text.DbIdentifier (dbIdentifierT, quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ContainsFlatten)
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Db.Type.Data.Tree (ColumnData(ColumnData))

import Polysemy.Hasql.Column.Tree (TableColumn, tableColumn)
import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (Name(Name), Selector(Selector))

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

class MapColumns (f :: (Kind.Tree -> Type) -> [Kind.Tree] -> Type) (a :: Type) (cols :: [Kind.Tree]) (cls :: Kind.Tree -> Constraint) where
  mapColumns :: (∀ x . cls x => Type.Column x -> a) -> f Type.Column cols -> CollapseTo f a

instance (
    HCollapse f,
    SListIN f cols,
    AllN (Prod f) cls cols,
    HAp f,
    All cls cols
  ) => MapColumns f a cols cls where
    mapColumns f cols =
      hcollapse @_ @_ @f @_ @a (hcmap (Proxy @cls) (K . f) cols)

class DataProduct (flatten :: Bool) (c :: Kind.Tree) where
  dataProduct :: ColumnPrefix -> Type.Column c -> [Data.Column]

instance (
    MapColumns NP [Data.Column] cols DataProductOrFlatten
  ) => DataProduct 'True ('Kind.Tree name effs ('Kind.Prod d cols)) where
  dataProduct prefix (Type.Tree _ (Type.Prod cols)) =
    join (mapColumns @NP @_ @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols)

instance (
    DataColumn ('Kind.Tree name effs tpe)
  ) => DataProduct 'False ('Kind.Tree name effs tpe) where
  dataProduct =
    pure .: dataColumn

class DataProductOrFlatten (c :: Kind.Tree) where
  dataProductOrFlatten :: ColumnPrefix -> Type.Column c -> [Data.Column]

instance (
    c ~ 'Kind.Tree name effs tpe,
    flatten ~ ContainsFlatten effs,
    DataProduct flatten c
  ) => DataProductOrFlatten c where
    dataProductOrFlatten =
      dataProduct @flatten @c

class DataDbType (t :: Kind.Node) where
  dataDbType :: ColumnPrefix -> Type.DbType t -> Data.DbType

instance (
    MapColumns NP [Data.Column] cols DataProductOrFlatten
  ) => DataDbType ('Kind.Prod d cols) where
  dataDbType prefix (Type.Prod cols) =
    Data.Prod (join (mapColumns @NP @_ @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols))

instance (
    MapColumns NS Data.Column cols DataColumn
  ) => DataDbType ('Kind.Sum d cols) where
  dataDbType prefix (Type.Sum cols) =
    Data.Sum (mapColumns @NS @_ @cols @DataColumn (dataColumn prefix) cols)

instance DataDbType ('Kind.Prim d) where
    dataDbType _ (Type.Prim _) =
      Data.Prim

class DataColumn (c :: Kind.Tree) where
  dataColumn :: ColumnPrefix -> Type.Column c -> Data.Column

instance (
    DataDbType t,
    FieldIdText name
  ) => DataColumn ('Kind.Tree name effs t) where
    dataColumn prefix (Type.Tree (ColumnData tpe options) dbType) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) tpe options dataType
      where
        dataType =
          dataDbType (addPrefix name prefix) dbType
        name =
          fieldIdText @name

class DataTable (c :: Kind.Tree) where
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
