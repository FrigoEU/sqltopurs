module Main where

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile, readTextFile)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Prelude (Unit, (<>), return, (==), bind, ($), const, show, (<<<), (<*>), (<$>))
import SqlToPurs.Codegen (header, full)
import SqlToPurs.Parsing (schemaP, functionsP)
import Text.Parsing.Parser (ParseError(ParseError), runParser)

setup :: YargsSetup
setup = usage "$0 -i Inputfile -o Outputfile" 
        <> example "$0 -i my.sql -o my.purs" "Turn SQL functions into PureScript functions"

main :: forall t124. Eff ( err :: EXCEPTION , console :: CONSOLE , fs :: FS | t124 ) Unit
main = runY setup $ go <$> yarg "i" ["in"]  (Just "Input File") (Right "Needs an input file") true
                       <*> yarg "o" ["out"] (Just "Output File") (Right "Needs an output file") true
                       <*> yarg "e" ["extra"] (Just "Extra File to be inlined") (Left "") true

go :: forall eff. String -> String -> String -> Eff (fs :: FS, console :: CONSOLE | eff) Unit
go i o e = runAff (log <<< ("Error: " <> _) <<< show) (const $ log "Done") do
  sql <- readTextFile UTF8 i
  extra <- if e == "" then return "" else readTextFile UTF8 e
  parsedFunctions <- either (\(ParseError {message}) -> throwError $ error $ "ParseError: " <> message) return $ runParser sql functionsP
  parsedSchemas <- either (\(ParseError {message}) -> throwError $ error $ "ParseError: " <> message) return $ runParser sql schemaP
  gen <- either (\e -> throwError $ error $ e) return $ full parsedSchemas parsedFunctions
  writeTextFile UTF8 o (header <> "\n" <> extra <> "\n" <> gen)
