module Main where

import Prelude (Unit, (<>), return, (==), bind, ($), const, show, (<<<), (<*>), (<$>))
import Control.Monad.Eff.Console (CONSOLE, log)
import SqlToPurs.Parsing (functionsP)
import SqlToPurs.Codegen (header, full)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (runAff)
import Node.FS (FS)
import Node.FS.Aff (writeTextFile, readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Either (either)
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Node.Yargs.Applicative (yarg, runY)
import Data.Maybe (Maybe(Just))
import Data.Either (Either(Left, Right), either)

setup :: YargsSetup
setup = usage "$0 -i Inputfile -o Outputfile" 
        <> example "$0 -i my.sql -o my.purs" "Turn SQL functions into PureScript functions"

main = runY setup $ go <$> yarg "i" ["in"]  (Just "Input File") (Right "Needs an input file") true
                       <*> yarg "o" ["out"] (Just "Output File") (Right "Needs an output file") true
                       <*> yarg "e" ["extra"] (Just "Extra File to be inlined") (Left "") true

go :: forall eff. String -> String -> String -> Eff (fs :: FS, console :: CONSOLE | eff) Unit
go i o e = runAff (log <<< ("Error: " <>) <<< show) (const $ log "Done") do
  sql <- readTextFile UTF8 i
  let purs = either (\(ParseError {message}) -> message) full $ runParser sql functionsP
  extra <- if e == "" then return "" else readTextFile UTF8 e
  {-- liftEff $ log purs --}
  writeTextFile UTF8 o (header <> "\n" <> extra <> "\n" <> purs)
