module Polysemy.Hasql.Table.QueryTable where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Column.Class (TableColumn)
import Polysemy.Hasql.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import qualified Polysemy.Hasql.Data.Where as Data
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Table (GenTable(genTable))
import Polysemy.Hasql.Where (Where(queryWhere))

class GenQuery (qrep :: *) (rep :: *) (q :: *) (d :: *) where
  genQuery :: Data.Where d q

instance (
    TableColumn rep d c,
    TableColumn qrep q qc,
    Where qc q c d
  ) =>
  GenQuery qrep rep q d where
    genQuery =
      queryWhere @qc @q @c @d


-- |Derives a full 'QueryTable' using a represenation type.
-- Given a record type:
--
-- @
-- data User = User { id :: Int, name :: Text } deriving Generic
-- @
--
-- the representation type could look like this:
--
-- @
-- data UserRep = UserRep { id :: PrimaryKey, name :: Prim } deriving Generic
-- @
--
-- indicating that the @id@ column should be created with a @primary key@ option, and the @name@ column should be an
-- ordinary primitive.
class GenQueryTable (qrep :: *) (rep :: *) (q :: *) (d :: *) where
  genQueryTable :: QueryTable q d

instance (
    TableColumn rep d c,
    TableColumn qrep q qc,
    GenTable rep d,
    QueryParams qc q,
    GenQuery qrep rep q d
  ) =>
  GenQueryTable qrep rep q d where
    genQueryTable =
      QueryTable (genTable @rep @d) (queryParams @qc @q) (genQuery @qrep @rep @q @d)

queryTable ::
  ∀ q d .
  GenQueryTable Auto Auto q d =>
  QueryTable q d
queryTable =
  genQueryTable @Auto @Auto
