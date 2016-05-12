module SqlToPurs.Parsing where

import Data.Array as A
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Foldable (foldl)
import Data.List (List, many, (:))
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Data.Monoid (mempty)
import Data.String (fromCharArray)
import Prelude (class Monad, return, (>>=), (<<<), (<$>), ($), bind)
import SqlToPurs.Model (SQLFunc(SQLFunc), Type(TimestampWithoutTimeZone, SqlDate, UUID, Text, Int, Boolean, Numeric), Var(Out, In))
import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser.Combinators (optional, optionMaybe, sepEndBy1, (<?>), between)
import Text.Parsing.Parser.String (anyChar, whiteSpace, string, char, oneOf, noneOf)

many' :: forall m. (Monad m) => ParserT String m Char -> ParserT String m String
many' p = fromCharArray <$> A.many p

dirP :: Parser String (String -> Type -> Var)
dirP = (string "IN" >>= \_ -> return In)
      <|> (string "OUT" >>= \_ -> return Out)
      <?> "IN or OUT"

word :: Parser String String
word = many' (noneOf [' ']) <?> "Variable Name"

betweenBrackets :: forall a m. (Monad m) => ParserT String m a -> ParserT String m a
betweenBrackets = between (string "(") (string ")")

digit :: Parser String Char
digit = oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] <?> "digit"

typeP :: Parser String Type
typeP = (string "boolean" >>= \_ -> return Boolean) 
        <|> (string "int" >>= \_ -> return Int)
        <|> (string "text" >>= \_ -> return Text)
        <|> (string "uuid" >>= \_ -> return UUID)
        <|> (string "date" >>= \_ -> return SqlDate)
        <|> (string "timestamp without time zone" >>= \_ -> return TimestampWithoutTimeZone)
        <|> numericP
        <?> "int, boolean, text, uuid or numeric(x,x)"
  where numericP = do string "numeric"
                      optional whiteSpace
                      betweenBrackets do
                        many' digit
                        char ','
                        many' digit
                      return Numeric

varP :: Parser String Var
varP = do
  dir <- dirP
  whiteSpace
  varName <- word
  whiteSpace
  t <- typeP
  return $ dir varName t

varsP :: Parser String (List Var)
varsP = betweenBrackets $ sepEndBy1 varP (optional whiteSpace *> char ',' *> optional whiteSpace)

setP :: Parser String Boolean
setP = isJust <$> optionMaybe ((string "RETURNS " <|> string "returns ") 
                               *> (string "SETOF " <|> string "setof ")
                               *> (string "RECORD" <|> string "record"))

createStatementP :: Parser String String
createStatementP = (string "CREATE FUNCTION " <|> string "create function ") 

functionP :: Parser String SQLFunc
functionP = do 
  createStatementP
  name <- word
  optional whiteSpace
  vars <- varsP
  optional whiteSpace
  set <- setP 
  return $ SQLFunc {name, vars, set}

functionsP :: Parser String (List SQLFunc)
functionsP = foldl f mempty <$> many (maybeP functionP)
  where
    f l (Nothing) = l
    f l (Just sf) = sf : l

maybeP :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Maybe a)
maybeP p = (p >>= return <<< Just) <|> (anyChar >>= \_ -> return Nothing)

{-- functionsP :: Parser String (List SQLFunc) --}
{-- functionsP = foldEitherP functionP anyChar f mempty --}
{--   where --}
{--     f l (Left sf) = sf : l --}
{--     f l (Right _) = l --}

{-- foldEitherP :: forall m a b c s. (Monad m) => --} 
{--                ParserT s m a -> ParserT s m b -> (c -> Either a b -> c) -> c -> ParserT s m c --}
{-- foldEitherP ap bp f init = run init --}
{--   where --} 
{--     run acc = do --}
{--       p <- (ap >>= return <<< Left) <|> (bp >>= return <<< Right) --}
{--       run (f acc p) --}


