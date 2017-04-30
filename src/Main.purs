module Main where

import Control.Apply ((*>))
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (throwException, EXCEPTION, error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Free (Free)
import Control.Monad.Trampoline (runTrampoline)
import Data.Either (Either(Left, Right), either)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(Just))
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile, readTextFile)
import Node.Yargs.Applicative (yarg, runY)
import Node.Yargs.Setup (YargsSetup, example, usage)
import Prelude (Unit, bind, const, pure, show, unit, ($), (<#>), (<$>), (<*>), (<<<), (<>), (==))
import SqlToPurs.Codegen (full, header, toEither)
import SqlToPurs.Parsing (schemaP, functionsP)
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position(..), initialPos)

setup :: YargsSetup
setup = usage "$0 -i Inputfile(s) -o Outputfile" 
        <> example "$0 -i my.sql -o my.purs -m Sweetapp.SQL" "Turn SQL functions into PureScript functions"

main :: forall t124. Eff ( exception :: EXCEPTION , console :: CONSOLE , fs :: FS | t124 ) Unit
main = runY setup $ go <$> yarg "i" ["in"]  (Just "Input File(s)") (Right "Needs at least one input file") true
                       <*> yarg "o" ["out"] (Just "Output File") (Right "Needs an output file") true
                       <*> yarg "m" ["module"] (Just "Module name") (Right "Needs a module name") true
                       <*> yarg "e" ["extra"] (Just "Extra File to be inlined") (Left "") true

go :: forall eff. String -> String -> String -> String -> Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
go i o m e = runAff (throwException <<< error <<< show) (const $ log "Done") (do
  let infiles = split (Pattern " ") i
  sql <- traverse (readTextFile UTF8) infiles <#> joinWith "\n"
  extra <- if e == "" then pure "" else readTextFile UTF8 e
  parsedFunctions <- either (\e -> throwError $ error $ "Parsing threw: " <> e)
                            (\r -> either (\e -> throwError $ error $ "ParseError: " <> show e) pure r)
                            (runStack sql functionsP)
  parsedSchemas <- either (\e -> throwError $ error $ "Parsing threw: " <> e)
                          (\r -> either (\e -> throwError $ error $ "ParseError: " <> show e) pure r)
                          (runStack sql schemaP)
  gen <- either (\e -> throwError $ error $ e) pure $ (toEither $ full parsedSchemas parsedFunctions)
  writeTextFile UTF8 o (header m <> "\n" <> extra <> "\n" <> gen))
    *> pure unit

runStack :: forall t8.
            String
            -> ParserT String (ExceptT String (Free Lazy)) t8
            -> Either String (Either ParseError t8)
runStack str parser =
  let ranParser = runParserT str parser :: ExceptT String (Free Lazy) (Either ParseError t8)
      ranExceptT = runExceptT ranParser :: Free Lazy (Either String (Either ParseError t8))
   in runTrampoline ranExceptT
