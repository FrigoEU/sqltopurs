module MyApp.SQL where
import Prelude ((<$>), return, bind, map, ($))
import Database.AnyDB (Connection, DB, query, Query(Query))
import Control.Monad.Aff (Aff)
import Database.AnyDB.SqlValue (toSql)
import Data.Foreign.Class (class IsForeign, readProp)
newtype UUID = UUID String

newtype Res0 = Res0 {id :: UUID, description :: String}
runRes0 :: Res0 -> {id :: UUID, description :: String}
runRes0 (Res0 a) = a
instance isForeignRes0 :: IsForeign Res0 where read obj = Res0 <$> ({id: _, description: _} <$> (readProp "id" obj) <*> (readProp "description" obj))
myfunc :: forall eff. Connection -> {myinvar :: UUID} -> Aff (db :: DB | eff) (Array {id :: UUID, description :: String})
myfunc conn {myinvar} = (map runRes0) <$> query (Query "select * from myfunc(?)") [toSql myinvar] conn

newtype Res1 = Res1 {id :: UUID, activityId :: UUID, datePoint :: Maybe SqlDate, anumber :: Number}
runRes1 :: Res1 -> {id :: UUID, activityId :: UUID, datePoint :: Maybe SqlDate, anumber :: Number}
runRes1 (Res1 a) = a
instance isForeignRes1 :: IsForeign Res1 where read obj = Res1 <$> ({id: _, activityId: _, datePoint: _, anumber: _} <$> (readProp "id" obj) <*> (readProp "activityId" obj) <*> (readProp "datePoint" obj >>= \p -> if isNull p then Nothing else Just p) <*> (readProp "anumber" obj))
myfunc2 :: forall eff. Connection -> {myinvar :: UUID} -> Aff (db :: DB | eff) ({id :: UUID, activityId :: UUID, datePoint :: Maybe SqlDate, anumber :: Number})
myfunc2 conn {myinvar} = (map runRes1) <$> query (Query "select * from myfunc2(?)") [toSql myinvar] conn

