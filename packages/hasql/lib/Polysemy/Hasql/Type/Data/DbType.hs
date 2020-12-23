module Polysemy.Hasql.Type.Data.DbType where

import Generics.SOP (All, Compose, K(K), NP, hcmap, hcollapse)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdTextRaw)
import qualified Text.Show as Show

import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind

data DbType (t :: Kind.DbType) :: * where
  Prim :: DbType ('Kind.Prim d)
  Prod :: NP Column cols -> DbType ('Kind.Prod d cols)
  Sum :: NP Column cols -> DbType ('Kind.Sum d cols)

instance Show (DbType ('Kind.Prim d)) where
  show Prim =
    "Prim"

instance (
  All (Compose Show Column) cols
  ) => Show (DbType ('Kind.Prod d cols)) where
  show (Prod cols) =
    [qt|Prod (#{hcollapse (hcmap (Proxy @(Compose Show Column)) (K . show @Text) cols)})|]

instance (
  All (Compose Show Column) cols
  ) => Show (DbType ('Kind.Sum d cols)) where
  show (Sum cols) =
    [qt|Prod (#{hcollapse (hcmap (Proxy @(Compose Show Column)) (K . show @Text) cols)})|]

data Column (t :: Kind.Column) :: * where
  Column :: Text -> ColumnOptions -> DbType col -> Column ('Kind.Column name eff col)

instance (Eq (DbType ('Kind.Prim d))) where
  _ == _ =
    True

instance (
  All (Compose Eq Column) cols
  ) => (Eq (DbType ('Kind.Prod d cols))) where
  Prod l == Prod r =
    l == r

instance (
  All (Compose Eq Column) cols
  ) => (Eq (DbType ('Kind.Sum d cols))) where
  Sum l == Sum r =
    l == r

instance (
    FieldIdText name,
    Show (DbType col)
  ) => Show (Column ('Kind.Column name eff col)) where
  show (Column tpe options t) =
    [qt|Column #{fieldIdTextRaw @name} #{tpe} (#{t}) (#{options})|]

deriving instance (
    Eq (DbType col)
  ) => Eq (Column ('Kind.Column name eff col))
