module Test.Main where

import Control.Apply ((*>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)
import Data.Bounded (top, bottom)
import Data.DateTime (DateTime(DateTime))
import Data.Either (Either(Right), either)
import Data.Foldable (foldl)
import Data.List (List(..), delete)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Database.Postgres (withClient, ConnectionInfo)
import Main (runStack)
import MyApp.SQL (querytest, inserttest)
import Prelude (bind, pure, show, unit, ($), (*>), (<>), (>>>))
import SqlToPurs.Codegen (genForeign, genFuncDef, genNewType, genNewtypeInstance, genTypeDecl, matchInVars, matchOutVars, tableToNewtypeName, tableToOutMatchedFields, toEither)
import SqlToPurs.Model (OutParams(Separate, FullTable), SQLField(SQLField), SQLFunc(..), SQLTable(SQLTable), ToGen(..), Type(Numeric, Date, Text, UUID), TypeAnn(NoAnn, NewType), Var(Var))
import SqlToPurs.Parsing (schemaP, functionsP)
import Test.Spec (it, describe)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.SqlTestModel (MyADT(Two))
import Text.Parsing.Parser (runParser)
import Unsafe.Coerce (unsafeCoerce)

main = run [consoleReporter] do
         parsingtest
         parsingfailtest
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

                    , "CREATE FUNCTION myfunc2 (IN my_invar posts.id%TYPE, OUT id posts.id%TYPE, OUT activityId posts.activityId%TYPE, OUT datePoint posts.datePoint%TYPE, OUT anumber posts.anumber%TYPE, OUT description activities.description%TYPE)"
                    , "  -- outer join activities;"
                    , "AS $$"
                    , "SELECT p.id, p.activityId, p.datePoint, p.anumber, a.description"
                    , "from posts p outer join activities a on p.id = a.id"
                    , "where p.id = my_invar;"
                    , "$$ LANGUAGE SQL;"
                    ]

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
             , toGen: SQLFuncApp
             , vars: { in: [Var "myinvar" "activities" "id"]
                     , out: FullTable "activities"}
             , set: true
             , outers: Nothing}

f2OutVars = [Var "id" "posts" "id", Var "activityId" "posts" "activityId", Var "datePoint" "posts" "datePoint", Var "anumber" "posts" "anumber", Var "description" "activities" "description"]
f2 :: SQLFunc
f2 = SQLFunc { name: "myfunc2"
             , toGen: SQLFuncApp
             , vars: { in: [Var "my_invar" "posts" "id"]
                     , out: Separate f2OutVars}
             , set: false
             , outers: Just (Cons "activities" Nil)}


parsingtest = describe "function parsing" do
  it "should parse the SQLFunc ADT's out of the sql script" do
    either (\e -> fail $ "Parsing Exception thrown: " <> e)
           (\r -> either (\e -> fail $ "Parsing failed: " <> show e) (shouldEqual [f1, f2]) r) 
           (runStack sql functionsP)
  it "should be able to parse functions without in or out vars" do
    let result = [SQLFunc {name: "queryAllActivities", toGen: SQLFuncApp, vars: {in: [], out: FullTable "activities"}, set: true, outers: Nothing}]
    let sql' = "CREATE FUNCTION queryAllActivities () RETURNS SETOF activities AS $$ select * from activities; $$ LANGUAGE SQL; "
    either (\e -> fail $ "Parsing Exception thrown: " <> e)
           (\r -> either (\e -> fail $ "Parsing failed: " <> show e) (shouldEqual result) r) 
           (runStack sql' functionsP)

parsingfailtest = describe "parsing without return" do
  it "is not supported" do
    let sql2 = joinWith "\n" [ "CREATE FUNCTION del(IN in_id posts.id%TYPE)"
                             , "AS $$"
                             , "delete from posts where id = in_id"
                             , "$$ LANGUAGE SQL"]
    expectError $
      either (\e -> fail $ "Parsing Exception thrown: " <> e)
             (\r -> either (\e -> fail $ "Parsing failed: " <> show e) (shouldEqual []) r)
             (runStack sql2 functionsP)


{-- parsingtest = pure unit --}
{-- schemaparsingtest = pure unit --}
{-- codegentest = pure unit --}

schemaparsingtest = describe "create table parsing" do
  it "should parse the create table statements correctly" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [activities, posts]) 
           $ runParser sql schemaP

codegentest = describe "codegen" do
  it "should generate the correct code" do
    f1InNamedFields <-
       either (\s -> fail s *> pure []) pure
       (toEither $ matchInVars [activities, posts] (unwrap f1).vars.in)
    let f1OutNamedFields = tableToOutMatchedFields activities

    f2InNamedFields <-
      either (\s -> fail s *> pure []) pure
      (toEither $ matchInVars [activities, posts] (unwrap f2).vars.in)
    f2OutNamedFields <-
      either (\s -> fail s *> pure []) pure
      (toEither $ matchOutVars [activities, posts] f2OutVars (Just (Cons "activities" Nil)))

  -- it "should generate a type declaration for SQLFunc ADT" do
    shouldEqual
      (genTypeDecl f1InNamedFields f1OutNamedFields (unwrap f1).set (unwrap f1).name)
      "myfunc :: forall eff obj. Client -> {myinvar :: UUID | obj} -> Aff (db :: DB | eff) (Array {id :: UUID, description :: String})"
    shouldEqual
      (genTypeDecl f2InNamedFields f2OutNamedFields (unwrap f2).set (unwrap f2).name)
      "myfunc2 :: forall eff obj. Client -> {my_invar :: PostId | obj} -> Aff (db :: DB | eff) (Maybe {id :: PostId, activityId :: UUID, datePoint :: Maybe Date, anumber :: Number, description :: Maybe String})"

  -- it "should generate a function definition" do
    shouldEqual (genFuncDef ((unwrap f1).name) (tableToNewtypeName activities) f1InNamedFields ((unwrap f1).set)) ("myfunc cl obj = (map unwrap) <$> query (Query \"select * from myfunc($1)\" :: Query (Array ActivitiesRec)) [toSql obj.myinvar] cl")
    shouldEqual (genFuncDef ((unwrap f2).name) "Res2" f2InNamedFields ((unwrap f2).set)) ("myfunc2 cl obj = (map unwrap) <$> queryOne (Query \"select * from myfunc2($1)\" :: Query Res2) [toSql (unwrap obj.my_invar)] cl")

  -- it "should generate a newtype" do
    shouldEqual
      (genNewType f1OutNamedFields (tableToNewtypeName activities))
      ("newtype ActivitiesRec = ActivitiesRec {id :: UUID, description :: String}")
    shouldEqual
      (genNewType f2OutNamedFields "Res2")
      "newtype Res2 = Res2 {id :: PostId, activityId :: UUID, datePoint :: Maybe Date, anumber :: Number, description :: Maybe String}"

  -- it "should generate a newtype instance" do
    shouldEqual
      (genNewtypeInstance "Res1")
      ("derive instance newtypeRes1 :: Newtype Res1 _ ")

  -- it "should generate a foreign instance, lowercasing the property names" do
    shouldEqual
      (genForeign f2OutNamedFields "Res2")
      "instance isSqlValueRes2 :: IsSqlValue Res2 where \n toSql a = toSql \"\"\n fromSql obj = Res2 <$> ({id: _, activityId: _, datePoint: _, anumber: _, description: _} <$> (PostId <$> (readSqlProp \"id\" obj :: F UUID)) <*> (readSqlProp \"activityid\" obj :: F UUID) <*> (readSqlProp \"datepoint\" obj :: F (Maybe Date)) <*> (readSqlProp \"anumber\" obj :: F Number) <*> (readSqlProp \"description\" obj :: F (Maybe String)))"

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
