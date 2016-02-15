module Main where

import Prelude (($), bind, const, show, (<>), (<<<))
import Control.Monad.Eff.Console (log)
import SqlToPurs.Parsing (functionsP)
import SqlToPurs.Codegen (full)
import Control.Monad.Aff (runAff)
import Node.FS.Aff (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Either (either)
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Control.Monad.Eff.Class (liftEff)

main = runAff (log <<< ("Parse Error: " <>) <<< show) (const $ log "Parsing Done") go

go = do
  sql <- readTextFile UTF8 "test.sql"
  let purs = either (\(ParseError {message}) -> message) full $ runParser sql functionsP
  liftEff $ log purs
  {-- writeTextFile UTF8 "test.purs" purs --}
