module Test.Main where

import Control.Apply ((*>))
import Data.Either (Either(Right), either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (joinWith)
import Prelude (show, bind, (<>), ($), unit, return)
import SqlToPurs.Codegen (toEither, genForeign, genRun, genNewType, genFuncDef, genTypeDecl)
import SqlToPurs.Model (SQLField(SQLField), OutParams(Separate, FullTable), Var(Var), SQLTable(SQLTable), SQLFunc(SQLFunc), Type(Numeric, SqlDate, Text, UUID))
import SqlToPurs.Parsing (schemaP, functionsP)
import Test.Spec (it, describe)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.Parser (runParser)

main = run [consoleReporter] $ foldl (*>) (return unit) [parsingtest, schemaparsingtest, codegentest]

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
                    , "  anumber numeric(2,2) NOT NULL"
                    , ");"

                          
                    , "CREATE FUNCTION myfunc (IN myinvar activities.id%TYPE)"
                    , "RETURNS SETOF activities"
                    , "AS $$"
                    , "  SELECT id, description"
                    , "  from activities"
                    , "  WHERE id = myinvar;"
                    , "$$ LANGUAGE SQL;"

                    , "CREATE FUNCTION myfunc2 (IN myinvar posts.id%TYPE, OUT posts.id%TYPE, OUT posts.activityId%TYPE, OUT posts.datePoint%TYPE, OUT posts.anumber%TYPE)"
                    , "AS $$"
                    , "SELECT id, activityId, datePoint, anumber"
                    , "from posts"
                    , "where id = myinvar;"
                    , "$$ LANGUAGE SQL;" ]

activities :: SQLTable
activities = SQLTable { name: "activities"
                      , fields: [ SQLField {name: "id", table: "activities", type: UUID, primarykey: true, notnull: false, newtype: Nothing }
                                , SQLField {name: "description", table: "activities", type: Text, primarykey: false, notnull: true, newtype: Nothing }]}
posts :: SQLTable
posts = SQLTable { name: "posts"
                 , fields: [ SQLField {name: "id", table: "posts", type: UUID, primarykey: true, notnull: false, newtype: Just "PostId"}
                           , SQLField {name: "activityId", table: "posts", type: UUID, primarykey: false, notnull: true, newtype: Nothing}
                           , SQLField {name: "datePoint", table: "posts", type: SqlDate, primarykey: false, notnull: false, newtype: Nothing }
                           , SQLField {name: "anumber", table: "posts", type: Numeric, primarykey: false, notnull: true, newtype: Nothing }]}

f1 :: SQLFunc
f1 = SQLFunc { name: "myfunc"
             , vars: { in: [Var (Just "myinvar") "activities" "id"]
                     , out: FullTable "activities"}
             , set: true}

f2 :: SQLFunc
f2 = SQLFunc { name: "myfunc2"
             , vars: { in: [Var (Just "myinvar") "posts" "id"]
                     , out: Separate [Var Nothing "posts" "id", Var Nothing "posts" "activityId", Var Nothing "posts" "datePoint", Var Nothing "posts" "anumber"]}
             , set: false}


parsingtest = describe "function parsing" do
  it "should parse the SQLFunc ADT's out of the sql script" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [f1, f2]) 
           $ runParser sql functionsP
  it "should be able to parse functions without in or out vars" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [SQLFunc {name: "queryAllActivities", vars: {in: [], out: FullTable "activities"}, set: true}]) 
           $ runParser "CREATE FUNCTION queryAllActivities () RETURNS SETOF activities AS $$ SELECT * from activities; $$ LANGUAGE SQL; " functionsP


{-- parsingtest = return unit --}
{-- schemaparsingtest = return unit --}
{-- codegentest = return unit --}

schemaparsingtest = describe "create table parsing" do
  it "should parse the create table statements correctly" do
    either (\e -> fail $ "Parsing failed: " <> show e) 
           (shouldEqual [activities, posts]) 
           $ runParser sql schemaP

codegentest = describe "codegen" do
  it "should generate a type declaration for SQLFunc ADT" do
    shouldEqual (toEither $ genTypeDecl [activities, posts] f1) $ Right "myfunc :: forall eff obj. Client -> {myinvar :: UUID | obj} -> Aff (db :: DB | eff) (Array {id :: UUID, description :: String})"
    shouldEqual (toEither $ genTypeDecl [activities, posts] f2) $
      Right "myfunc2 :: forall eff obj. Client -> {myinvar :: PostId | obj} -> Aff (db :: DB | eff) (Maybe {id :: PostId, activityId :: UUID, datePoint :: Maybe SqlDate, anumber :: Number})"

  it "should generate a function definition" do
    shouldEqual (genFuncDef [activities, posts] "Res1" f1) ("myfunc cl {myinvar} = (map runRes1) <$> query (Query \"select * from myfunc(?)\") [toSql myinvar] cl")
    shouldEqual (genFuncDef [activities, posts] "Res2" f2) ("myfunc2 cl {myinvar: (PostId myinvar)} = (map runRes2) <$> queryOne (Query \"select * from myfunc2(?)\") [toSql myinvar] cl")

  it "should generate a newtype" do
    shouldEqual (toEither $ genNewType "Res1" [activities, posts] (getOutVars f1)) (Right "newtype Res1 = Res1 {id :: UUID, description :: String}")
    shouldEqual (toEither $ genNewType "Res2" [activities, posts] (getOutVars f2)) (Right "newtype Res2 = Res2 {id :: PostId, activityId :: UUID, datePoint :: Maybe SqlDate, anumber :: Number}")

  it "should generate a run function" do
    shouldEqual (toEither $ genRun "Res1" [activities, posts] (getOutVars f1)) ( Right $ joinWith "\n" [ "runRes1 :: Res1 -> {id :: UUID, description :: String}"
                                                                                                            , "runRes1 (Res1 a) = a"])

  it "should generate a foreign instance" do
    shouldEqual (toEither $ genForeign "Res2" [activities, posts] (getOutVars f2)) (Right "instance isForeignRes2 :: IsForeign Res2 where read obj = Res2 <$> ({id: _, activityId: _, datePoint: _, anumber: _} <$> (PostId <$> (readProp \"id\" obj :: F UUID)) <*> (readProp \"activityId\" obj :: F UUID) <*> (runNull <$> (readProp \"datePoint\" obj :: F (Null SqlDate))) <*> (readProp \"anumber\" obj :: F Number))")

getOutVars :: SQLFunc -> OutParams
getOutVars (SQLFunc {vars: {out}}) = out
