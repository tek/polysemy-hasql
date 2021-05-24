module Polysemy.Hasql.Column.DataColumn where

import Generics.SOP (All, AllN, CollapseTo, HAp, HCollapse(hcollapse), K(K), NP, Prod, hcmap)
import Generics.SOP.Constraint (SListIN)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText, FieldId (NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Text.DbIdentifier (dbIdentifierT, quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ContainsFlatten)
import qualified Polysemy.Db.Type.Data.Tree as Type

import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (Name(Name), Selector(Selector))
import Polysemy.Hasql.Tree.Table (ColumnData(ColumnData), TableCon, TableNode, TableRoot, TableTree, tableRoot)

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
  BasePrefix name -> TypePrefix [text|(#{name}).#{quotedDbId segment}|]
  TypePrefix prefix -> TypePrefix [text|#{prefix}.#{quotedDbId segment}|]

prefixed :: Text -> ColumnPrefix -> Text
prefixed name = \case
  InitPrefix -> quotedDbId name
  TablePrefix -> quotedDbId name
  BasePrefix prefix -> [text|(#{prefix}).#{quotedDbId name}|]
  TypePrefix prefix -> [text|#{prefix}.#{quotedDbId name}|]

mapColumns ::
  ∀ k (f :: (k -> Type) -> [k] -> Type) (a :: Type) (cols :: [k]) (cls :: k -> Constraint) n .
  HCollapse f =>
  SListIN f cols =>
  AllN (Prod f) cls cols =>
  HAp f =>
  (∀ x . cls x => n x -> a) ->
  f n cols ->
  CollapseTo f a
mapColumns f cols =
  hcollapse @_ @_ @f @_ @a (hcmap (Proxy @cls) (K . f) cols)

class DataProduct (flatten :: Bool) (c :: Kind.Tree) where
  dataProduct :: ColumnPrefix -> TableTree c -> [Data.Column]

instance (
    All DataProductOrFlatten cols
  ) => DataProduct 'True ('Kind.Tree name effs ('Kind.Prod d cols)) where
    dataProduct prefix (Type.Tree _ (Type.Prod _ cols)) =
      join (mapColumns @_ @NP @_ @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols)

instance (
    DataColumn ('Kind.Tree name effs tpe)
  ) => DataProduct 'False ('Kind.Tree name effs tpe) where
    dataProduct =
      pure .: dataColumn

class DataProductOrFlatten (c :: Kind.Tree) where
  dataProductOrFlatten :: ColumnPrefix -> TableTree c -> [Data.Column]

instance (
    c ~ 'Kind.Tree name effs tpe,
    flatten ~ ContainsFlatten effs,
    DataProduct flatten c
  ) => DataProductOrFlatten c where
    dataProductOrFlatten =
      dataProduct @flatten @c

class DataDbCon (con :: Kind.Con) where
  dataDbCon :: ColumnPrefix -> TableCon con -> Data.Column

instance (
    All (DataProduct 'False) cols,
    FieldIdText name
  ) => DataDbCon ('Kind.Con num name cols) where
    dataDbCon prefix (Type.Con cols) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) name def dbType
      where
        name =
          fieldIdText @name
        dbType =
          Data.Prod (join (mapColumns @_ @NP @_ @cols @(DataProduct 'False) (dataProduct @'False newPrefix) cols))
        newPrefix =
          addPrefix name prefix

type family ConUnaName (cname :: FieldId) (fname :: FieldId) :: FieldId where
  ConUnaName _ ('NamedField name) =
    'NamedField name
  ConUnaName cname _ =
    cname

instance (
    DataDbNode node,
    name ~ ConUnaName cname fname,
    FieldIdText name
  ) => DataDbCon ('Kind.ConUna num cname ('Kind.Tree fname effs node)) where
    dataDbCon prefix (Type.ConUna (Type.Tree (ColumnData tpe options) tree)) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) tpe options (dataDbNode newPrefix tree)
      where
        newPrefix =
          addPrefix name prefix
        name =
          fieldIdText @name

class DataDbNode (t :: Kind.Node) where
  dataDbNode :: ColumnPrefix -> TableNode t -> Data.DbType

instance (
    All DataProductOrFlatten cols
  ) => DataDbNode ('Kind.Prod d cols) where
  dataDbNode prefix (Type.Prod _ cols) =
    Data.Prod (join (mapColumns @_ @NP @_ @cols @DataProductOrFlatten (dataProductOrFlatten prefix) cols))

instance (
    All DataDbCon cols
  ) => DataDbNode ('Kind.SumProd d cols) where
  dataDbNode prefix (Type.SumProd _ cols) =
    Data.Prod (indexColumn : mapColumns @_ @NP @_ @cols @DataDbCon (dataDbCon prefix) cols)
    where
      indexColumn =
        Data.Column (Name indexName) (Selector (prefixed indexName prefix)) "bigint" def Data.Prim
      indexName =
        "sum__index"

instance DataDbNode ('Kind.Prim d) where
    dataDbNode _ (Type.Prim _) =
      Data.Prim

class DataColumn (c :: Kind.Tree) where
  dataColumn :: ColumnPrefix -> TableTree c -> Data.Column

instance (
    DataDbNode t,
    FieldIdText name
  ) => DataColumn ('Kind.Tree name effs t) where
    dataColumn prefix (Type.Tree (ColumnData tpe options) dbType) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) tpe options dataType
      where
        dataType =
          dataDbNode (addPrefix name prefix) dbType
        name =
          fieldIdText @name

class DataTable (c :: Kind.Tree) where
  dataTable :: TableTree c -> Data.Column

instance (
    DataColumn c
  ) => DataTable c where
  dataTable =
    dataColumn @c InitPrefix

class TableStructure (rep :: *) (d :: *) where
  tableStructure :: Data.Column

instance (
    TableRoot rep d c,
    DataTable c
  ) => TableStructure rep d where
    tableStructure =
      dataTable (tableRoot @rep @d)
