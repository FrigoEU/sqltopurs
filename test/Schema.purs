module MyApp.SQL where
import Prelude ((<$>), map, (<*>))
import Database.Postgres (Client, DB, query, Query(Query), queryOne)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Foreign (F)
import Data.Newtype (class Newtype, unwrap)
import Database.Postgres.SqlValue (toSql, readSqlProp, fromSql, class IsSqlValue)
import Test.SqlTestModel (UUID, MyADT)
import Data.Date (Date)
import Data.Time (Time)
import Data.DateTime (DateTime)

querytest :: forall eff . Client -> Aff (db :: DB | eff) (Maybe {id :: UUID, d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time, myadt :: Maybe MyADT})
querytest cl  = (map unwrap) <$> queryOne (Query "select * from querytest()" :: Query TesttableRec) [] cl
newtype TesttableRec = TesttableRec {id :: UUID, d :: Date, twotz :: DateTime, t :: Time, mt :: Maybe Time, myadt :: Maybe MyADT}
derive instance newtypeTesttableRec :: Newtype TesttableRec _ 
instance isSqlValueTesttableRec :: IsSqlValue TesttableRec where 
 toSql a = toSql ""
 fromSql obj = TesttableRec <$> ({id: _, d: _, twotz: _, t: _, mt: _, myadt: _} <$> (readSqlProp "id" obj :: F UUID) <*> (readSqlProp "d" obj :: F Date) <*> (readSqlProp "twotz" obj :: F DateTime) <*> (readSqlProp "t" obj :: F Time) <*> (readSqlProp "mt" obj :: F (Maybe Time)) <*> (readSqlProp "myadt" obj :: F (Maybe MyADT)))

inserttest :: forall eff obj. Client -> {d :: Date, twotz :: DateTime, t :: Time, myadt :: Maybe MyADT, mt :: Maybe Time | obj} -> Aff (db :: DB | eff) (Maybe {id :: UUID})
inserttest cl obj = (map unwrap) <$> queryOne (Query "select * from inserttest($1,$2,$3,$4,$5)" :: Query Res1) [toSql obj.d, toSql obj.twotz, toSql obj.t, toSql obj.myadt, toSql obj.mt] cl
newtype Res1 = Res1 {id :: UUID}
derive instance newtypeRes1 :: Newtype Res1 _ 
instance isSqlValueRes1 :: IsSqlValue Res1 where 
 toSql a = toSql ""
 fromSql obj = Res1 <$> ({id: _} <$> (readSqlProp "id" obj :: F UUID))

