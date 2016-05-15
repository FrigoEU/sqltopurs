module SqlToPurs.Parsing where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Bind (join)
import Data.Array (many, snoc, length, filter, some)
import Data.Foldable (foldl)
import Data.List (toUnfoldable)
import Data.Maybe (maybe, Maybe(Nothing, Just), isJust)
import Data.Monoid (mempty)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(Tuple))
import Prelude (pure, Unit, class Monad, return, ($), bind, (<$>), (>>=), unit, (>), (&&), not, (/=), (==), (>>>), (<<<))
import SqlToPurs.Model (SQLTable(SQLTable), SQLField(SQLField), OutParams(FullTable, Separate), Var(Var), SQLFunc(SQLFunc), Type(TimestampWithoutTimeZone, SqlDate, UUID, Text, Int, Boolean, Numeric))
import Text.Parsing.Parser (fail, ParserT, Parser)
import Text.Parsing.Parser.Combinators (sepEndBy, optionMaybe, manyTill, optional, sepEndBy1, (<?>), try, between)
import Text.Parsing.Parser.String (anyChar, string, whiteSpace, char, oneOf)
import Text.Parsing.Parser.Token (alphaNum)

data Dir = In | Out

some' :: forall m. (Monad m) => ParserT String m Char -> ParserT String m String
some' p = fromCharArray <$> some p

dirP :: Parser String Dir
dirP = (string "IN" >>= \_ -> return In)
      <|> (string "OUT" >>= \_ -> return Out)
      <?> "IN or OUT"

word :: Parser String String
word = some' alphaNum <?> "Variable Name"

betweenBrackets :: forall a m. (Monad m) => ParserT String m a -> ParserT String m a
betweenBrackets = between (string "(") (string ")")

varP :: Parser String Var
varP = try do name <- word
              whiteSpace
              tablename <- word
              char '.'
              fieldname <- word
              string "%TYPE"
              return $ Var (Just name) tablename fieldname
       <|>
       do 
          tablename <- word
          char '.'
          fieldname <- word
          string "%TYPE"
          return $ Var Nothing tablename fieldname

varAndDirP :: Parser String (Tuple Dir Var)
varAndDirP = do
  dir <- dirP
  whiteSpace
  var <- varP
  return $ Tuple dir var

varsP :: Parser String (Array (Tuple Dir Var))
varsP = betweenBrackets $ toUnfoldable <$> (sepEndBy varAndDirP (optional whiteSpace *> char ',' *> optional whiteSpace))

returnsP :: Parser String (Maybe (Tuple Boolean String))
returnsP = (try >>> maybeP)
       ( do string "RETURNS " <|> string "returns "
            string "SETOF " <|> string "setof "
            str <- word
            return (Tuple true str)
         <|>
         do string "RETURNS " <|> string "returns "
            str <- word
            return (Tuple false str))


createFunctionP :: Parser String String
createFunctionP = (string "CREATE FUNCTION " <|> string "create function ") 

functionP :: Parser String SQLFunc
functionP = do 
  createFunctionP
  name <- word
  optional whiteSpace
  vars <- varsP
  optional whiteSpace
  returns <- returnsP
  let set = maybe false (\(Tuple b _) -> b) returns
  let returnsRecord = maybe false (\(Tuple _ str) -> str == "record") returns
  let returnsFullTable = join $ (\(Tuple _ str) -> if (str /= "record") then Just str else Nothing) <$> returns
  let invars  = runVar <$> filter isIn vars
  let outvars = runVar <$> filter (not isIn) vars
  if (isJust returnsFullTable && length outvars > 0) then fail "Can't have both return table and out vars"
                                                     else return unit
  return $ SQLFunc {name, vars: {in: invars, out: maybe (Separate outvars) FullTable returnsFullTable }, set}
    where 
      isIn :: Tuple Dir Var -> Boolean
      isIn (Tuple In v) = true
      isIn _            = false
      runVar :: Tuple Dir Var -> Var
      runVar (Tuple _ v) = v


functionsP :: Parser String (Array SQLFunc)
functionsP = manyMaybe functionP

manyMaybe :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Array a)
manyMaybe p = foldl f mempty <$> many (maybeP p)
  where
    {-- f :: (Array a) --} 
    f l (Nothing) = l
    f l (Just sf) = snoc l sf


maybeP :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Maybe a)
maybeP p = (p >>= return <<< Just) <|> (anyChar >>= \_ -> return Nothing)

---------------

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
        <?> "int, boolean, text, uuid, numeric(x,x), date or timestamp without time zone"
  where numericP = do string "numeric"
                      optional whiteSpace
                      betweenBrackets do
                        some' digit
                        char ','
                        some' digit
                      return Numeric

createTableP :: Parser String String
createTableP = do (string "create table " <|> string "create table ")
                  word

schemaP :: Parser String (Array SQLTable)
schemaP = manyMaybe tableP

tableP :: Parser String SQLTable
tableP = do
  name <- createTableP
  whiteSpace
  string "("
  whiteSpace
  fields <- toUnfoldable <$> (sepEndBy1 (fieldP name) (optional whiteSpace *> char ',' *> optional whiteSpace))
  whiteSpace
  string ")"
  optional (string ";")
  return $ SQLTable {name, fields}

commentP :: Parser String Unit
commentP = string "--" *> manyTill anyChar (string "\n") *> pure unit

fieldP :: String -> Parser String SQLField
fieldP table = do
  name <- word
  whiteSpace
  t <- typeP
  whiteSpace
  primarykey <- optionMaybe (string "PRIMARY KEY" <|> string "primary key")
  notnull <- optionMaybe (string "NOT NULL" <|> string "not null")
  optional whiteSpace
  optional (string "PRIMARY KEY" <|> string "primary key" <|> string "NOT NULL" <|> string "not null")
  optional whiteSpace
  return $ SQLField {name, table, "type": t, primarykey: isJust primarykey, notnull: isJust notnull}

