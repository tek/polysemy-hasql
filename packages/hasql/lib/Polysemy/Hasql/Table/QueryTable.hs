module Polysemy.Hasql.Table.QueryTable where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Db.Tree ()
import Polysemy.Hasql.Tree.Table (TableRoot)
import Polysemy.Hasql.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import qualified Polysemy.Hasql.Data.Where as Data
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Table (GenTable(genTable))
import Polysemy.Hasql.Where (Where(queryWhere))

class GenQuery (qrep :: *) (rep :: *) (q :: *) (d :: *) where
  genQuery :: Data.Where d q

instance (
    TableRoot rep d dTree,
    TableRoot qrep q qTree,
    Where qrep qTree q dTree d
  ) =>
  GenQuery qrep rep q d where
    genQuery =
      queryWhere @qrep @qTree @_ @dTree

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
class (
    GenTable rep d,
    GenQuery qrep rep q d
  ) => GenQueryTable (qrep :: *) (rep :: *) (q :: *) (d :: *) where
    genQueryTable :: QueryTable q d

instance (
    TableRoot rep d dTree,
    TableRoot qrep q qTree,
    GenTable rep d,
    QueryParams qTree q,
    GenQuery qrep rep q d
  ) => GenQueryTable qrep rep q d where
    genQueryTable =
      QueryTable (genTable @rep @d) (queryParams @qTree @q) (genQuery @qrep @rep @q @d)

queryTable ::
  ∀ q d .
  GenQueryTable Auto Auto q d =>
  QueryTable q d
queryTable =
  genQueryTable @Auto @Auto
