module Main where

import Prelude (($), bind, const, show, (<>), (<<<), (<*>), (<$>))
import Control.Monad.Eff.Console (log)
import SqlToPurs.Parsing (functionsP)
import SqlToPurs.Codegen (full)
import Control.Monad.Aff (runAff)
import Node.FS.Aff (writeTextFile, readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Either (either)
import Text.Parsing.Parser (ParseError(ParseError), runParser)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Node.Yargs.Applicative (yarg, runY)
import Data.Maybe (Maybe(Just))
import Data.Either (Either(Right), either)

setup :: YargsSetup
setup = usage "$0 -i Inputfile -o Outputfile" 
        <> example "$0 -i my.sql -o my.purs" "Turn SQL functions into PureScript functions"

main = runY setup $ go <$> yarg "i" ["in"]  (Just "Input File") (Right "Needs an input file") true
                       <*> yarg "o" ["out"] (Just "Output File") (Right "Needs an output file") true

go i o = runAff (log <<< ("Error: " <>) <<< show) (const $ log "Done") do
  sql <- readTextFile UTF8 i
  let purs = either (\(ParseError {message}) -> message) full $ runParser sql functionsP
  {-- liftEff $ log purs --}
  writeTextFile UTF8 o purs
