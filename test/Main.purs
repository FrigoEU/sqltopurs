module Test.Main where

import Prelude (bind, (<>), ($), unit, return)
import Control.Apply ((*>))
import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.List (List(Cons,Nil))
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Data.Either (either)

import SqlToPurs.Model (SQLFunc(SQLFunc), Type(Boolean, Int, Numeric, TimestampWithoutTimeZone), Var(Out, In))
import SqlToPurs.Parsing (functionsP)
import SqlToPurs.Codegen (NamedRecord(NamedRecord), genForeign, genRun, genNewType, genFuncDef, genTypeDecl)

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec (it, describe)
import Test.Spec.Assertions       (shouldEqual, fail)

main = run [consoleReporter] $ foldl (*>) (return unit) [parsingtest, codegentest]

parsingtest = describe "function parsing" do
  it "should parse the SQLFunc ADT's out of the sql script" do
    let sql = joinWith "\n" [ "blablablablababla;"
                            , "CREATE FUNCTION myfunc (IN myinvar boolean, OUT myvar numeric(2,2), OUT myvar2 timestamp without time zone)"
                            , "RETURNS SETOF record"
                            , "AS $$"
                            , "  SELECT id, enrollment_id, amount, paid"
                            , "  from invoices"
                            , "  where amount >= am"
                            , "  ;"
                            , "$$ LANGUAGE SQL;"
                            , "CREATE FUNCTION myfunc (IN myinvar boolean, IN mysecondvar int, OUT myvar numeric(2,2), OUT mysecondoutvar int)"
                            , "returns uuid"
                            , "AS $$"
                            , "SELECT id, enrollment_id, amount, paid"
                            , "from invoices"
                            , "where amount >= am"
                            , ";"
                            , "$$ LANGUAGE SQL;" ]
    let f1 = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) (Cons (Out "myvar2" TimestampWithoutTimeZone) Nil)), set: true}
    let f2 = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (In "mysecondvar" Int) (Cons (Out "myvar" Numeric) (Cons (Out "mysecondoutvar" Int) Nil))), set: false}
    either (\(ParseError {message}) -> fail $ "Parsing failed: " <> message) 
           (shouldEqual (Cons f2 (Cons f1 Nil))) 
           $ runParser sql functionsP

codegentest = describe "codegen" do
  it "should generate a type declaration for SQLFunc ADT" do
    let f = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) Nil), set: true}
    shouldEqual (genTypeDecl f) ("myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})")
  it "should generate a function definition" do
    let f = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) (Cons (In "myinvar2" Int) Nil)), set: true}
    shouldEqual (genFuncDef "Res1" f) ("myfunc conn {myinvar, myinvar2} = (map runRes1) <$> query (Query \"select * from myfunc(?,?)\") [toSql myinvar, toSql myinvar2] conn")
  it "should generate a newtype" do
    let nr = NamedRecord "Res1" (Cons (Out "myinvar" Boolean) Nil)
    shouldEqual (genNewType nr) ("newtype Res1 = Res1 {myinvar :: Boolean}")
  it "should generate a run function" do
    let nr = NamedRecord "Res1" (Cons (Out "myinvar" Boolean) Nil)
    shouldEqual (genRun nr) (joinWith "\n" [ "runRes1 :: Res1 -> {myinvar :: Boolean}"
                                           , "runRes1 (Res1 a) = a"])

  it "should generate a foreign instance" do
    let nr = NamedRecord "Res1" (Cons (Out "myinvar" Boolean) Nil)
    shouldEqual (genForeign nr) (joinWith "\n" [ "instance isForeignRes1 :: IsForeign Res1 where"
                                               , "  read obj = do"
                                               , "    myinvar <- readProp \"myinvar\" obj"
                                               , "    return $ Res1 {myinvar}"])
