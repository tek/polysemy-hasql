module Sqel.Data.QuerySchema where

import Sqel.Data.Codec (Encoder)
import Sqel.Data.Sql (ToSql (toSql))
import Sqel.Sql.Select (SelectFragment)
import Sqel.Sql.SelectQuery (SelectQuery (SelectQuery))

data QuerySchema q a =
  QuerySchema {
    frags :: [SelectFragment],
    encoder :: Encoder q
  }
  deriving stock (Generic)

emptyQuerySchema :: QuerySchema () a
emptyQuerySchema =
  QuerySchema mempty mempty

instance ToSql (SelectQuery (QuerySchema q a)) where
  toSql (SelectQuery (QuerySchema frags _)) =
    toSql frags
