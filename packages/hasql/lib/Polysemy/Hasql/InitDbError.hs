module Polysemy.Hasql.InitDbError where

import Polysemy.Db.Data.InitDbError (InitDbError(InitDbError))

initDbError ::
  Members [eff !! e, Error InitDbError] r =>
  Show e =>
  Sem (eff : r) a ->
  Sem r a
initDbError =
  resumeHoistError (InitDbError . show)
