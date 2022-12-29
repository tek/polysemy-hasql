module Sqel.PgType where

import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Exon
import Exon (exon)
import Lens.Micro.Extras (view)
import Type.Errors (ErrorMessage)

import Sqel.Class.MatchView (MatchProjection)
import Sqel.Data.Codec (Codec (Codec), FullCodec)
import Sqel.Data.Dd (Dd, DdK, DdType)
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName (PgColumnName),
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
import Sqel.Data.PgTypeName (PgTableName, pgCompName, pgTableName)
import Sqel.Data.ProjectionSchema (ProjectionSchema (ProjectionSchema))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Data.Term (Comp, CompInc (Merge), DdTerm (DdTerm), Struct (Comp, Prim))
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))
import Sqel.ReifyDd (ReifyDd (reifyDd))
import Sqel.SOP.Error (Quoted)
import Sqel.Sql.Prepared (dollar)
import Sqel.Text.Quote (dquote)

pgColumn ::
  DdTerm ->
  ([(PgColumnName, ColumnType)], [(PgColumnName, StructureType)], Map PgTypeRef PgComposite, [NonEmpty PgColumnName])
pgColumn = \case
  DdTerm name _ (Prim t unique constr) ->
    ([(name, ColumnPrim t unique constr)], [(name, StructurePrim t unique constr)], mempty, [pure name])
  DdTerm name _ (Comp typeName c i sub) ->
    case comp typeName c i sub of
      (compType@(PgComposite cname _), struct, types, False, sels) ->
        (colType, structType, Map.insert ref compType types, (name <|) <$> sels)
        where
          colType = [(name, ColumnComp ref)]
          structType = [(name, StructureComp cname struct)]
          ref = pgCompRef cname
      (PgComposite _ (PgColumns columns), PgStructure struct, types, True, sels) ->
        (columns, struct, types, sels)

comp ::
  Text ->
  Comp ->
  CompInc ->
  [DdTerm] ->
  (PgComposite, PgStructure, Map PgTypeRef PgComposite, Bool, [NonEmpty PgColumnName])
comp typeName _ i sub =
  (compType, structType, Map.unions (view _3 <$> cols), i == Merge, view _4 =<< cols)
  where
    compType = PgComposite compName (PgColumns (view _1 =<< cols))
    structType = PgStructure (view _2 =<< cols)
    compName = pgCompName typeName
    cols = pgColumn <$> sub

-- TODO this used to dquote the @names@ as well but it appears to fail for the sum index field
mkSelector :: NonEmpty PgColumnName -> Selector
mkSelector =
  Selector . Sql . \case
    [PgColumnName name] -> dquote name
    root :| names -> [exon|(##{dquote root}).##{Text.intercalate "." (coerce names)}|]

-- TODO use CommaSep
mkValues :: PgStructure -> [Sql]
mkValues (PgStructure base) =
  snd (mapAccumL mkCol (1 :: Int) base)
  where
    mkCol (n :: Int) = \case
      (_, StructurePrim _ _ _) -> (n + 1, [sql|##{dollar n}|])
      (_, StructureComp _ (PgStructure cols)) ->
        (newN, [sql|row(#{Exon.intercalate ", " sub})|])
        where
          (newN, sub) =
            mapAccumL mkCol n cols

mkTable ::
  PgColumnName ->
  Maybe PgTableName ->
  PgColumns ->
  Map PgTypeRef PgComposite ->
  [NonEmpty PgColumnName] ->
  PgStructure ->
  PgTable a
mkTable (PgColumnName name) tableName cols types selectors struct =
  PgTable (fromMaybe (pgTableName name) tableName) cols types (TableSelectors (mkSelector <$> selectors)) values struct
  where
    values = TableValues (mkValues struct)

toTable :: DdTerm -> PgTable a
toTable = \case
  DdTerm name tableName (Prim t unique constr) ->
    mkTable name tableName cols [] [pure name] struct
    where
      cols = PgColumns [(name, ColumnPrim t unique constr)]
      struct = PgStructure [(name, StructurePrim t unique constr)]
  DdTerm _ tableName (Comp typeName c i sub) ->
    mkTable (pgColumnName typeName) tableName cols types paths struct
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

class CheckedProjection' (check :: Maybe Void) (s :: DdK) where
  checkedProjection' :: Dd s -> ProjectionSchema (DdType s) table

instance CheckedProjection' 'Nothing s where
  checkedProjection' _ = ProjectionSchema

class CheckedProjection (query :: DdK) (table :: DdK) where
  checkedProjection :: Dd query -> ProjectionSchema (DdType query) (DdType table)

type CheckProjectionStuck :: ErrorMessage
type CheckProjectionStuck =
  "Could not validate projection fields since there is not enough type information available." %
  "You are most likely missing a constraint for " <> Quoted "CheckedProjection" <> "."

instance (
    MatchProjection proj table match,
    CheckedProjection' match proj
  ) => CheckedProjection proj table where
    checkedProjection = checkedProjection' @match

projectionSchema ::
  ∀ proj table .
  CheckedProjection proj table =>
  Dd proj ->
  Dd table ->
  ProjectionSchema (DdType proj) (DdType table)
projectionSchema proj _ =
  checkedProjection @proj @table proj
