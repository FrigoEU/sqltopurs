module MyApp.SQL where
import Prelude ((<$>), map, (<*>))
import Database.Postgres (Client, DB, query, Query(Query), queryOne)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Foreign (F)
import Data.Foreign.Null (Null, runNull)
import Database.Postgres.SqlValue (toSql)
import Data.Foreign.Class (class IsForeign, readProp)
newtype UUID = UUID String

queryAllActivities :: forall eff . Client -> Aff (db :: DB | eff) (Array {id :: ActivityId, description :: String})
queryAllActivities cl  = (map runRes0) <$> query (Query "select * from queryAllActivities()") [] cl
newtype Res0 = Res0 {id :: ActivityId, description :: String}
runRes0 :: Res0 -> {id :: ActivityId, description :: String}
runRes0 (Res0 a) = a
instance isForeignRes0 :: IsForeign Res0 where read obj = Res0 <$> ({id: _, description: _} <$> (ActivityId <$> (readProp "id" obj :: F UUID)) <*> (readProp "description" obj :: F String))

queryAllPlaces :: forall eff . Client -> Aff (db :: DB | eff) (Array {id :: PlaceId, description :: String})
queryAllPlaces cl  = (map runRes1) <$> query (Query "select * from queryAllPlaces()") [] cl
newtype Res1 = Res1 {id :: PlaceId, description :: String}
runRes1 :: Res1 -> {id :: PlaceId, description :: String}
runRes1 (Res1 a) = a
instance isForeignRes1 :: IsForeign Res1 where read obj = Res1 <$> ({id: _, description: _} <$> (PlaceId <$> (readProp "id" obj :: F UUID)) <*> (readProp "description" obj :: F String))

queryNewMessages :: forall eff obj. Client -> {last :: TimestampWithTimeZone, userId :: UserId | obj} -> Aff (db :: DB | eff) (Array {id :: MessageId, postId :: PostId, fromUserId :: UserId, toUserId :: UserId, at :: TimestampWithTimeZone, body :: String})
queryNewMessages cl {last, userId: (UserId userId)} = (map runRes2) <$> query (Query "select * from queryNewMessages(?,?)") [toSql last, toSql userId] cl
newtype Res2 = Res2 {id :: MessageId, postId :: PostId, fromUserId :: UserId, toUserId :: UserId, at :: TimestampWithTimeZone, body :: String}
runRes2 :: Res2 -> {id :: MessageId, postId :: PostId, fromUserId :: UserId, toUserId :: UserId, at :: TimestampWithTimeZone, body :: String}
runRes2 (Res2 a) = a
instance isForeignRes2 :: IsForeign Res2 where read obj = Res2 <$> ({id: _, postId: _, fromUserId: _, toUserId: _, at: _, body: _} <$> (MessageId <$> (readProp "id" obj :: F UUID)) <*> (PostId <$> (readProp "postId" obj :: F UUID)) <*> (UserId <$> (readProp "fromUserId" obj :: F UUID)) <*> (UserId <$> (readProp "toUserId" obj :: F UUID)) <*> (readProp "at" obj :: F TimestampWithTimeZone) <*> (readProp "body" obj :: F String))

queryPostsByActivity :: forall eff obj. Client -> {activityId :: ActivityId | obj} -> Aff (db :: DB | eff) (Array {id :: PostId, userId :: UserId, activityId :: ActivityId, placeId :: PlaceId, datePoint :: Maybe SqlDate, timePoint :: String, firstName :: String})
queryPostsByActivity cl {activityId: (ActivityId activityId)} = (map runRes3) <$> query (Query "select * from queryPostsByActivity(?)") [toSql activityId] cl
newtype Res3 = Res3 {id :: PostId, userId :: UserId, activityId :: ActivityId, placeId :: PlaceId, datePoint :: Maybe SqlDate, timePoint :: String, firstName :: String}
runRes3 :: Res3 -> {id :: PostId, userId :: UserId, activityId :: ActivityId, placeId :: PlaceId, datePoint :: Maybe SqlDate, timePoint :: String, firstName :: String}
runRes3 (Res3 a) = a
instance isForeignRes3 :: IsForeign Res3 where read obj = Res3 <$> ({id: _, userId: _, activityId: _, placeId: _, datePoint: _, timePoint: _, firstName: _} <$> (PostId <$> (readProp "id" obj :: F UUID)) <*> (UserId <$> (readProp "userId" obj :: F UUID)) <*> (ActivityId <$> (readProp "activityId" obj :: F UUID)) <*> (PlaceId <$> (readProp "placeId" obj :: F UUID)) <*> (runNull <$> (readProp "datePoint" obj :: F (Null SqlDate))) <*> (readProp "timePoint" obj :: F String) <*> (readProp "firstName" obj :: F String))

insertPost :: forall eff obj. Client -> {userId :: UserId, activityId :: ActivityId, placeId :: PlaceId, datePoint :: Maybe SqlDate, timePoint :: String | obj} -> Aff (db :: DB | eff) (Maybe {id :: PostId})
insertPost cl {userId: (UserId userId), activityId: (ActivityId activityId), placeId: (PlaceId placeId), datePoint, timePoint} = (map runRes4) <$> queryOne (Query "select * from insertPost(?,?,?,?,?)") [toSql userId, toSql activityId, toSql placeId, toSql datePoint, toSql timePoint] cl
newtype Res4 = Res4 {id :: PostId}
runRes4 :: Res4 -> {id :: PostId}
runRes4 (Res4 a) = a
instance isForeignRes4 :: IsForeign Res4 where read obj = Res4 <$> ({id: _} <$> (PostId <$> (readProp "id" obj :: F UUID)))

insertMessage :: forall eff obj. Client -> {postId :: PostId, fromUserId :: UserId, toUserId :: UserId, at :: TimestampWithTimeZone, body :: String | obj} -> Aff (db :: DB | eff) (Maybe {id :: MessageId})
insertMessage cl {postId: (PostId postId), fromUserId: (UserId fromUserId), toUserId: (UserId toUserId), at, body} = (map runRes5) <$> queryOne (Query "select * from insertMessage(?,?,?,?,?)") [toSql postId, toSql fromUserId, toSql toUserId, toSql at, toSql body] cl
newtype Res5 = Res5 {id :: MessageId}
runRes5 :: Res5 -> {id :: MessageId}
runRes5 (Res5 a) = a
instance isForeignRes5 :: IsForeign Res5 where read obj = Res5 <$> ({id: _} <$> (MessageId <$> (readProp "id" obj :: F UUID)))

dbLogin :: forall eff obj. Client -> {email :: String, password :: String, now :: TimestampWithTimeZone | obj} -> Aff (db :: DB | eff) (Maybe {sessionId :: SessionId})
dbLogin cl {email, password, now} = (map runRes6) <$> queryOne (Query "select * from dbLogin(?,?,?)") [toSql email, toSql password, toSql now] cl
newtype Res6 = Res6 {sessionId :: SessionId}
runRes6 :: Res6 -> {sessionId :: SessionId}
runRes6 (Res6 a) = a
instance isForeignRes6 :: IsForeign Res6 where read obj = Res6 <$> ({sessionId: _} <$> (SessionId <$> (readProp "sessionId" obj :: F UUID)))

getSession :: forall eff obj. Client -> {sessionId :: SessionId | obj} -> Aff (db :: DB | eff) (Maybe {id :: SessionId, userId :: UserId, createdAt :: TimestampWithTimeZone})
getSession cl {sessionId: (SessionId sessionId)} = (map runRes7) <$> queryOne (Query "select * from getSession(?)") [toSql sessionId] cl
newtype Res7 = Res7 {id :: SessionId, userId :: UserId, createdAt :: TimestampWithTimeZone}
runRes7 :: Res7 -> {id :: SessionId, userId :: UserId, createdAt :: TimestampWithTimeZone}
runRes7 (Res7 a) = a
instance isForeignRes7 :: IsForeign Res7 where read obj = Res7 <$> ({id: _, userId: _, createdAt: _} <$> (SessionId <$> (readProp "id" obj :: F UUID)) <*> (UserId <$> (readProp "userId" obj :: F UUID)) <*> (readProp "createdAt" obj :: F TimestampWithTimeZone))

