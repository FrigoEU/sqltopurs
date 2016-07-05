module MyApp.SQL where
import Prelude ((<$>), map, (<*>))
import Database.Postgres (Client, DB, query, Query(Query), queryOne)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Foreign (F)
import Database.Postgres.SqlValue (toSql, readSqlProp, fromSql, class IsSqlValue)
import Test.SqlTestModel (UUID)
import Data.Date (Date)
import Data.Time (Time)
import Data.DateTime (DateTime)

querytest :: forall eff . Client -> Aff (db :: DB | eff) (Maybe {id :: UUID, d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time})
querytest cl  = (map runRes0) <$> queryOne (Query "select * from querytest()") [] cl
newtype Res0 = Res0 {id :: UUID, d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time}
runRes0 :: Res0 -> {id :: UUID, d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time}
runRes0 (Res0 a) = a
instance isSqlValueRes0 :: IsSqlValue Res0 where 
 toSql a = toSql ""
 fromSql obj = Res0 <$> ({id: _, d: _, twotz: _, t: _, mt: _} <$> (readSqlProp "id" obj :: F UUID) <*> (readSqlProp "d" obj :: F Date) <*> (readSqlProp "twotz" obj :: F DateTime) <*> (readSqlProp "t" obj :: F Time) <*> (readSqlProp "mt" obj :: F (Maybe Time)))

inserttest :: forall eff obj. Client -> {d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time | obj} -> Aff (db :: DB | eff) (Maybe {id :: UUID})
inserttest cl {d, twotz, t, mt} = (map runRes1) <$> queryOne (Query "select * from inserttest($1,$2,$3,$4)") [toSql d, toSql twotz, toSql t, toSql mt] cl
newtype Res1 = Res1 {id :: UUID}
runRes1 :: Res1 -> {id :: UUID}
runRes1 (Res1 a) = a
instance isSqlValueRes1 :: IsSqlValue Res1 where 
 toSql a = toSql ""
 fromSql obj = Res1 <$> ({id: _} <$> (readSqlProp "id" obj :: F UUID))

