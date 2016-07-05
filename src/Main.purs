module Main where

import Control.Apply ((*>))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (throwException, EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free)
import Control.Monad.Trampoline (runTrampoline)
import Data.Either (Either(Left, Right), either)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(Just))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile, readTextFile)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Prelude (unit, Unit, (<<<), (<>), pure, ($), bind, show, (==), const, (<*>), (<$>))
import SqlToPurs.Codegen (header, full)
import SqlToPurs.Parsing (schemaP, functionsP)
import Text.Parsing.Parser (ParseError, ParserT, PState(PState), runParserT)
import Text.Parsing.Parser.Pos (initialPos)

setup :: YargsSetup
setup = usage "$0 -i Inputfile -o Outputfile" 
        <> example "$0 -i my.sql -o my.purs" "Turn SQL functions into PureScript functions"

main :: forall t124. Eff ( err :: EXCEPTION , console :: CONSOLE , fs :: FS | t124 ) Unit
main = runY setup $ go <$> yarg "i" ["in"]  (Just "Input File") (Right "Needs an input file") true
                       <*> yarg "o" ["out"] (Just "Output File") (Right "Needs an output file") true
                       <*> yarg "e" ["extra"] (Just "Extra File to be inlined") (Left "") true

go :: forall eff. String -> String -> String -> Eff (fs :: FS, console :: CONSOLE, err :: EXCEPTION | eff) Unit
go i o e = runAff (throwException <<< error <<< show) (const $ log "Done") (do
  sql <- readTextFile UTF8 i
  extra <- if e == "" then pure "" else readTextFile UTF8 e
  parsedFunctions <- either (\e -> throwError $ error $ "ParseError: " <> show e) pure $ runStack sql functionsP
  parsedSchemas <- either (\e -> throwError $ error $ "ParseError: " <> show e) pure $ runStack sql schemaP
  gen <- either (\e -> throwError $ error $ e) pure $ full parsedSchemas parsedFunctions
  writeTextFile UTF8 o (header <> "\n" <> extra <> "\n" <> gen))
    *> pure unit

runStack :: forall t8 t9. t9 -> ParserT t9 (Free Lazy) t8 -> Either ParseError t8
runStack s = runTrampoline <<< runParserT (PState {input: s, position: initialPos})
