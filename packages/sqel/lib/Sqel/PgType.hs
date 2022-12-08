module Sqel.PgType where

import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Exon
import Exon (exon)
import Lens.Micro.Extras (view)

import Sqel.Data.Codec (Codec (Codec), FullCodec)
import Sqel.Data.Dd (Dd, DdK, DdType)
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName,
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgStructure (PgStructure),
  PgTable (PgTable),
  PgTypeRef,
  StructureType (StructureComp, StructurePrim),
  TableSelectors (TableSelectors),
  TableValues (TableValues),
  pgColumnName,
  pgCompRef,
  )
import Sqel.Data.PgTypeName (pgCompName, pgTableName)
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Data.Term (Comp, CompInc (Merge), DdTerm (DdTerm), Struct (Comp, Prim))
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))
import Sqel.ReifyDd (ReifyDd (reifyDd))
import Sqel.Sql.Prepared (dollar)
import Sqel.Text.DbIdentifier (dbIdentifierT, quotedDbId)
import Sqel.Text.Quote (dquote)

pgColumn ::
  DdTerm ->
  ([(PgColumnName, ColumnType)], [(PgColumnName, StructureType)], Map PgTypeRef PgComposite, [NonEmpty Text])
pgColumn = \case
  DdTerm name (Prim t opt) ->
    ([(pgColumnName name, ColumnPrim t opt)], [(pgColumnName name, StructurePrim t opt)], mempty, [pure name])
  DdTerm name (Comp typeName c i sub) ->
    case comp typeName c i sub of
      (compType@(PgComposite cname _), struct, types, False, sels) ->
        (colType, structType, Map.insert ref compType types, (name <|) <$> sels)
        where
          colType = [(pgColumnName name, ColumnComp ref)]
          structType = [(pgColumnName name, StructureComp ref struct)]
          ref = pgCompRef cname
      (PgComposite _ (PgColumns columns), PgStructure struct, types, True, sels) ->
        (columns, struct, types, sels)

comp ::
  Text ->
  Comp ->
  CompInc ->
  [DdTerm] ->
  (PgComposite, PgStructure, Map PgTypeRef PgComposite, Bool, [NonEmpty Text])
comp typeName _ i sub =
  (compType, structType, Map.unions (view _3 <$> cols), i == Merge, view _4 =<< cols)
  where
    compType = PgComposite compName (PgColumns (view _1 =<< cols))
    structType = PgStructure (view _2 =<< cols)
    compName = pgCompName typeName
    cols = pgColumn <$> sub

-- TODO this used to dquote the @names@ as well but it appears to fail for the sum index field
mkSelector :: NonEmpty Text -> Selector
mkSelector =
  Selector . Sql . \case
    [name] -> quotedDbId name
    root :| names -> [exon|(#{dquote root}).#{Text.intercalate "." (dbIdentifierT <$> names)}|]

-- TODO use CommaSep
mkValues :: PgStructure -> [Sql]
mkValues (PgStructure base) =
  snd (mapAccumL mkCol (1 :: Int) base)
  where
    mkCol (n :: Int) = \case
      (_, StructurePrim _ _) -> (n + 1, [sql|##{dollar n}|])
      (_, StructureComp _ (PgStructure cols)) ->
        (newN, [sql|row(#{Exon.intercalate ", " sub})|])
        where
          (newN, sub) =
            mapAccumL mkCol n cols

mkTable ::
  Text ->
  PgColumns ->
  Map PgTypeRef PgComposite ->
  [NonEmpty Text] ->
  PgStructure ->
  PgTable a
mkTable name cols types selectors struct =
  PgTable (pgTableName name) cols types (TableSelectors (mkSelector <$> selectors)) values struct
  where
    values = TableValues (mkValues struct)

toTable :: DdTerm -> PgTable a
toTable = \case
  DdTerm name (Prim t opt) ->
    mkTable name cols [] [pure name] struct
    where
      cols = PgColumns [(pgColumnName name, ColumnPrim t opt)]
      struct = PgStructure [(pgColumnName name, StructurePrim t opt)]
  DdTerm _ (Comp typeName c i sub) ->
    mkTable typeName cols types paths struct
    where
      (PgComposite _ cols, struct, types, _, paths) = comp typeName c i sub

pgTable ::
  ∀ s .
  ReifyDd s =>
  Dd s ->
  PgTable (DdType s)
pgTable dd =
  toTable (reifyDd dd)

type MkTableSchema :: DdK -> Constraint
class MkTableSchema table where
  tableSchema :: Dd table -> TableSchema (DdType table)

instance (
    ReifyDd table,
    ReifyCodec FullCodec table (DdType table)
  ) => MkTableSchema table where
  tableSchema tab =
    TableSchema (pgTable tab) (row ^. #decodeValue) (params ^. #encodeValue)
    where
      Codec params row = reifyCodec @FullCodec tab
