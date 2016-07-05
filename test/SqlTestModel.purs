module Test.SqlTestModel where

import Data.Generic (class Generic, gShow, gEq)
import Database.Postgres.SqlValue (fromSql, toSql, class IsSqlValue)
import Prelude (class Eq, class Show, (<$>))

newtype UUID = UUID String
derive instance genericUUID :: Generic UUID
instance eqUUID :: Eq UUID where eq = gEq
instance showUUID :: Show UUID where show = gShow
instance isSqlValueUUID :: IsSqlValue UUID where
  toSql (UUID s) = toSql s
  fromSql o = UUID <$> fromSql o
