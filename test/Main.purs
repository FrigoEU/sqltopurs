module Test.Main where

import Prelude
import Control.Apply ((*>))
import Control.Monad.Eff.Console
import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.List (List(Cons,Nil))
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Data.Either (Either(Left,Right), either)

import SqlToPurs.Model
import SqlToPurs.Parsing
import SqlToPurs.Codegen

import Test.Spec.Runner           (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec                  (describe, pending, it)
import Test.Spec.Assertions       (shouldEqual, fail)

main = run [consoleReporter] $ foldl (*>) (return unit) [parsingtest, codegentest]

parsingtest = describe "function parsing" do
  it "should parse the SQLFunc ADT's out of the sql script" do
    let sql = joinWith "\n" [ "blablablablababla;"
                            , "CREATE FUNCTION myfunc (IN myinvar boolean, OUT myvar numeric(2,2))"
                            , "RETURNS SETOF record"
                            , "AS $$"
                            , "  SELECT id, enrollment_id, amount, paid"
                            , "  from invoices"
                            , "  where amount >= am"
                            , "  ;"
                            , "$$ LANGUAGE SQL;"
                            , "CREATE FUNCTION myfunc (IN myinvar boolean, IN mysecondvar int, OUT myvar numeric(2,2), OUT mysecondoutvar int)"
                            , "AS $$"
                            , "SELECT id, enrollment_id, amount, paid"
                            , "from invoices"
                            , "where amount >= am"
                            , ";"
                            , "$$ LANGUAGE SQL;" ]
    let f1 = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) Nil), set: true}
    let f2 = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (In "mysecondvar" Int) (Cons (Out "myvar" Numeric) (Cons (Out "mysecondoutvar" Int) Nil))), set: false}
    either (\(ParseError {message}) -> fail $ "Parsing failed: " <> message) 
           (shouldEqual (Cons f2 (Cons f1 Nil))) 
           $ runParser sql functionsP

codegentest = describe "codegen" do
  it "should generate a type declaration for SQLFunc ADT" do
    let f = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) Nil), set: true}
    shouldEqual (typeDecl f) ("myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})")
  it "should generate a function definition" do
    let f = SQLFunc {name: "myfunc", vars: Cons (In "myinvar" Boolean) (Cons (Out "myvar" Numeric) (Cons (In "myinvar2" Int) Nil)), set: true}
    shouldEqual (funcDef f) ("myfunc conn {myinvar, myinvar2} = query \"select * from myfunc(?,?)\" [toSql myinvar, toSql myinvar2] conn")
