module Test.Main where

import Control.Apply ((*>))
import Data.Bounded (top, bottom)
import Data.DateTime (DateTime(DateTime))
import Data.Either (Either(Right), either)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Database.Postgres (withClient, ConnectionInfo)
import MyApp.SQL (querytest, inserttest)
import Prelude (show, bind, (<>), ($), unit, pure)
import SqlToPurs.Codegen (toEither, genForeign, genRun, genNewType, genFuncDef, genTypeDecl)
import SqlToPurs.Model (TypeAnn(NoAnn, NewType), SQLField(SQLField), OutParams(Separate, FullTable), Var(Var), SQLTable(SQLTable), SQLFunc(SQLFunc), Type(Numeric, Date, Text, UUID))
import SqlToPurs.Parsing (schemaP, functionsP)
import Test.Spec (it, describe)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.SqlTestModel (MyADT(Two))
import Text.Parsing.Parser (runParser)

main = run [consoleReporter] do
         parsingtest
         schemaparsingtest
         codegentest 
         sqltest

sql :: String
sql = joinWith "\n" [ "blablablablababla;"
                    , "create table activities ("
                    , "  id uuid PRIMARY KEY,"
                    , "  description text NOT NULL"
                    , ");"
                    , ""

                    , "create table posts ("
                    , "  id uuid PRIMARY KEY/* newtype PostId */,"
                    , "  activityId uuid UNIQUE NOT NULL, -- ignore 'unique' test"
                    , "  datePoint date, --comment test"
                    , "  anumber numeric(2,2) NOT NULL,"
                    , "  FOREIGN KEY (activityId) REFERENCES activities(id)"
                    , ");"


                    , "CREATE FUNCTION myfunc ("
                    , "IN myinvar activities.id%TYPE)"
                    , "RETURNS SETOF activities"
                    , "AS $$"
                    , "  SELECT id, description"
                    , "  from activities"
                    , "  WHERE id = myinvar;"
                    , "$$ LANGUAGE SQL;"

                    , "CREATE FUNCTION myfunc2 (IN my_invar posts.id%TYPE, OUT posts.id%TYPE, OUT posts.activityId%TYPE, OUT posts.datePoint%TYPE, OUT posts.anumber%TYPE, OUT activities.description%TYPE)"
                    , "  -- outer join activities;"
                    , "AS $$"
                    , "SELECT p.id, p.activityId, p.datePoint, p.anumber, a.description"
                    , "from posts p outer join activities a on p.id = a.id"
                    , "where p.id = my_invar;"
                    , "$$ LANGUAGE SQL;" ]

activities :: SQLTable
activities = SQLTable { name: "activities"
                      , fields: [ SQLField {name: "id", table: "activities", type: UUID, primarykey: true, notnull: false, newtype: NoAnn }
                                , SQLField {name: "description", table: "activities", type: Text, primarykey: false, notnull: true, newtype: NoAnn }]}
posts :: SQLTable
posts = SQLTable { name: "posts"
                 , fields: [ SQLField {name: "id", table: "posts", type: UUID, primarykey: true, notnull: false, newtype: NewType "PostId"}
                           , SQLField {name: "activityId", table: "posts", type: UUID, primarykey: false, notnull: true, newtype: NoAnn}
                           , SQLField {name: "datePoint", table: "posts", type: Date, primarykey: false, notnull: false, newtype: NoAnn }
                           , SQLField {name: "anumber", table: "posts", type: Numeric, primarykey: false, notnull: true, newtype: NoAnn }]}

f1 :: SQLFunc
f1 = SQLFunc { name: "myfunc"
             , vars: { in: [Var (Just "myinvar") "activities" "id"]
                     , out: FullTable "activities"}
             , set: true
             , outers: Nothing}

f2 :: SQLFunc
f2 = SQLFunc { name: "myfunc2"
             , vars: { in: [Var (Just "my_invar") "posts" "id"]
                     , out: Separate [Var Nothing "posts" "id", Var Nothing "posts" "activityId", Var Nothing "posts" "datePoint", Var Nothing "posts" "anumber", Var Nothing "activities" "description"]}
             , set: false
             , outers: Just (Cons "activities" Nil)}


parsingtest = describe "function parsing" do
  it "should parse the SQLFunc ADT's out of the sql script" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [f1, f2]) 
           $ runParser sql functionsP
  it "should be able to parse functions without in or out vars" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [SQLFunc {name: "queryAllActivities", vars: {in: [], out: FullTable "activities"}, set: true, outers: Nothing}]) 
           $ runParser "CREATE FUNCTION queryAllActivities () RETURNS SETOF activities AS $$ select * from activities; $$ LANGUAGE SQL; " functionsP


{-- parsingtest = pure unit --}
{-- schemaparsingtest = pure unit --}
{-- codegentest = pure unit --}

schemaparsingtest = describe "create table parsing" do
  it "should parse the create table statements correctly" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [activities, posts]) 
           $ runParser sql schemaP

codegentest = describe "codegen" do
  it "should generate a type declaration for SQLFunc ADT" do
    shouldEqual (toEither $ genTypeDecl [activities, posts] f1) $ Right "myfunc :: forall eff obj. Client -> {myinvar :: UUID | obj} -> Aff (db :: DB | eff) (Array {id :: UUID, description :: String})"
    shouldEqual (toEither $ genTypeDecl [activities, posts] f2) $
      Right "myfunc2 :: forall eff obj. Client -> {my_invar :: PostId | obj} -> Aff (db :: DB | eff) (Maybe {id :: PostId, activityId :: UUID, datePoint :: Maybe Date, anumber :: Number, description :: Maybe String})"

  it "should generate a function definition" do
    shouldEqual (genFuncDef [activities, posts] "Res1" f1) ("myfunc cl {myinvar} = (map runRes1) <$> query (Query \"select * from myfunc($1)\") [toSql myinvar] cl")
    shouldEqual (genFuncDef [activities, posts] "Res2" f2) ("myfunc2 cl {my_invar: (PostId my_invar)} = (map runRes2) <$> queryOne (Query \"select * from myfunc2($1)\") [toSql my_invar] cl")

  it "should generate a newtype" do
    shouldEqual
      (toEither $ genNewType "Res1" [activities, posts] (getOutVars f1) Nothing)
      (Right "newtype Res1 = Res1 {id :: UUID, description :: String}")
    shouldEqual
      (toEither $ genNewType "Res2" [activities, posts] (getOutVars f2) (Just (Cons "activities" Nil)))
      (Right "newtype Res2 = Res2 {id :: PostId, activityId :: UUID, datePoint :: Maybe Date, anumber :: Number, description :: Maybe String}")

  it "should generate a run function" do
    shouldEqual
      (toEither $ genRun "Res1" [activities, posts] (getOutVars f1) Nothing)
      (Right $ joinWith "\n" [ "runRes1 :: Res1 -> {id :: UUID, description :: String}" , "runRes1 (Res1 a) = a"])

  it "should generate a foreign instance, lowercasing the property names" do
    shouldEqual (toEither $ genForeign "Res2" [activities, posts] (getOutVars f2) (Just (Cons "activities" Nil))) (Right "instance isSqlValueRes2 :: IsSqlValue Res2 where \n toSql a = toSql \"\"\n fromSql obj = Res2 <$> ({id: _, activityId: _, datePoint: _, anumber: _, description: _} <$> (PostId <$> (readSqlProp \"id\" obj :: F UUID)) <*> (readSqlProp \"activityid\" obj :: F UUID) <*> (readSqlProp \"datepoint\" obj :: F (Maybe Date)) <*> (readSqlProp \"anumber\" obj :: F Number) <*> (readSqlProp \"description\" obj :: F (Maybe String)))")

getOutVars :: SQLFunc -> OutParams
getOutVars (SQLFunc {vars: {out}}) = out

localConnInfo :: ConnectionInfo
localConnInfo = {host: "localhost", db: "sqltopurstest", port: 5432, user: "", password: ""}

sqltest = describe "insert and retrieve row" do
  it "should retrieve the same stuff we put in" do
    let d = top
    let t = bottom
    let twotz = DateTime d t
    let mt = Nothing
    let myadt = Just Two
    withClient localConnInfo \c -> do 
      inserttest c {d, t, twotz, myadt, mt}
      res <- querytest c 
      maybe 
        (fail "No record found")
        (\{d: d1, t: t1, twotz: twotz1, myadt: myadt1, mt: mt1} -> do
          d `shouldEqual` d1
          t `shouldEqual` t1
          twotz `shouldEqual` twotz1
          myadt `shouldEqual` myadt1
          mt `shouldEqual` mt1
          )
        res
