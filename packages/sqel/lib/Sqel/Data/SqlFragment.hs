module Sqel.Data.SqlFragment where

import qualified Exon

import Sqel.Data.Sql (ToSql (toSql))

newtype CommaSep a =
  CommaSep { unCommaSep :: a }
  deriving stock (Eq, Show, Generic)

instance ToSql a => ToSql (CommaSep [a]) where
  toSql (CommaSep a) =
    Exon.intercalate ", " (toSql <$> a)

newtype Delete a =
  Delete { unDelete :: a }
  deriving stock (Eq, Show, Generic)

newtype From a =
  From { unFrom :: a }
  deriving stock (Eq, Show, Generic)

newtype Insert a =
  Insert { unInsert :: a }
  deriving stock (Eq, Show, Generic)

newtype Into a =
  Into { unInto :: a }
  deriving stock (Eq, Show, Generic)

newtype Returning a =
  Returning { unReturning :: a }
  deriving stock (Eq, Show, Generic)

newtype Select a =
  Select { unSelect :: a }
  deriving stock (Eq, Show, Generic)

newtype SelectQuery a =
  SelectQuery { unSelectQuery :: a }
  deriving stock (Eq, Show, Generic)

newtype Update a =
  Update { unUpdate :: a }
  deriving stock (Eq, Show, Generic)

newtype Create a =
  Create { unSelect :: a }
  deriving stock (Eq, Show, Generic)
