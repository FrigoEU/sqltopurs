module SqlToPurs.Parsing where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Array (many, snoc, length, filter, some)
import Data.Foldable (foldMap, foldl)
import Data.List (toUnfoldable)
import Data.Maybe (isNothing, maybe, Maybe(Nothing, Just), isJust)
import Data.Monoid (mempty)
import Data.String (fromCharArray, contains, toLower)
import Data.Tuple (Tuple(Tuple))
import Prelude (class Monad, Unit, ($), bind, (<$>), unit, pure, (>>=), (<<<), (>), (&&), not, (/=), (==), (>>>))
import SqlToPurs.Model (SQLTable(SQLTable), SQLField(SQLField), OutParams(FullTable, Separate), Var(Var), SQLFunc(SQLFunc), Type(SqlTime, TimestampWithTimeZone, TimestampWithoutTimeZone, SqlDate, UUID, Text, Int, Boolean, Numeric))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (option, sepBy, optionMaybe, optional, choice, manyTill, sepBy1, (<?>), try, between)
import Text.Parsing.Parser.String (anyChar, string, whiteSpace, char, oneOf)
import Text.Parsing.Parser.Token (alphaNum)

data Dir = In | Out

some' :: forall m. (Monad m) => ParserT String m Char -> ParserT String m String
some' p = fromCharArray <$> some p

dirP :: forall m. (Monad m) => ParserT String m Dir
dirP = (string "IN" >>= \_ -> pure In)
      <|> (string "OUT" >>= \_ -> pure Out)
      <?> "IN or OUT"

word :: forall m. (Monad m) => ParserT String m String
word = some' alphaNum <?> "Variable Name"

betweenBrackets :: forall a m. (Monad m) => ParserT String m a -> ParserT String m a
betweenBrackets = between (string "(") (string ")")

varP :: forall m. (Monad m) => ParserT String m Var
varP = try do name <- word
              whiteSpace
              tablename <- word
              char '.'
              fieldname <- word
              string "%TYPE"
              pure $ Var (Just name) tablename fieldname
       <|>
       do 
          tablename <- word
          char '.'
          fieldname <- word
          string "%TYPE"
          pure $ Var Nothing tablename fieldname

varAndDirP :: forall m. (Monad m) => ParserT String m (Tuple Dir Var)
varAndDirP = do
  dir <- dirP
  whiteSpace
  var <- varP
  pure $ Tuple dir var

varsP :: forall m. (Monad m) => ParserT String m (Array (Tuple Dir Var))
varsP = betweenBrackets $ toUnfoldable <$> (sepBy varAndDirP (optional whiteSpace *> char ',' *> optional whiteSpace))

returnsP :: forall m. (Monad m) => ParserT String m (Maybe (Tuple Boolean String))
returnsP = (try >>> maybeP)
       ( do string "RETURNS SETOF " <|> string "returns setof "
            str <- word
            pure (Tuple true str)
         <|>
         do string "RETURNS " <|> string "returns "
            str <- word
            pure (Tuple false str))


createFunctionP :: forall m. (Monad m) => ParserT String m String
createFunctionP = (string "CREATE FUNCTION " <|> string "create function ") 

functionP :: forall m. (Monad m) => ParserT String m SQLFunc
functionP = do 
  createFunctionP
  name <- word
  optional whiteSpace
  vars <- varsP
  optional whiteSpace
  returns <- returnsP
  let set = maybe false (\(Tuple b _) -> b) returns
  let returnsRecord = maybe false (\(Tuple _ str) -> str == "record") returns
  let returnsFullTable = returns >>= (\(Tuple _ str) -> if (str /= "record") then Just str else Nothing)
  let invars  = runVar <$> filter isIn vars
  let outvars = runVar <$> filter (not isIn) vars
  if (isJust returnsFullTable && length outvars > 0) then fail "Can't have both pure table and out vars"
                                                     else pure unit
  out <- if isNothing returnsFullTable && length outvars == 0 
            then fail "Function is not returning anything, not supported. If deleting, just pure the id"
            else pure $ maybe (Separate outvars) FullTable returnsFullTable 
  pure $ SQLFunc {name, vars: {in: invars, out}, set}
    where 
      isIn :: Tuple Dir Var -> Boolean
      isIn (Tuple In v) = true
      isIn _            = false
      runVar :: Tuple Dir Var -> Var
      runVar (Tuple _ v) = v


functionsP :: forall m. (Monad m) => ParserT String m (Array SQLFunc)
functionsP = manyMaybe functionP

manyMaybe :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Array a)
manyMaybe p = foldl f mempty <$> many (maybeP p)
  where
    {-- f :: (Array a) --} 
    f l (Nothing) = l
    f l (Just sf) = snoc l sf


maybeP :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Maybe a)
maybeP p = (p >>= pure <<< Just) <|> (anyChar >>= \_ -> pure Nothing)

---------------

digit :: forall m. (Monad m) => ParserT String m Char
digit = oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] <?> "digit"

typeP :: forall m. (Monad m) => ParserT String m Type
typeP = (string "boolean" >>= \_ -> pure Boolean) 
        <|> (string "int" >>= \_ -> pure Int)
        <|> (string "text" >>= \_ -> pure Text)
        <|> (string "uuid" >>= \_ -> pure UUID)
        <|> (string "date" >>= \_ -> pure SqlDate)
        <|> (string "time" >>= \_ -> pure SqlTime)
        <|> (string "timestamp without time zone" >>= \_ -> pure TimestampWithoutTimeZone)
        <|> (string "timestamp with time zone" >>= \_ -> pure TimestampWithTimeZone)
        <|> numericP
        <?> "int, boolean, text, uuid, numeric(x,x), date, timestamp with time zone or timestamp without time zone"
  where numericP = do string "numeric"
                      optional whiteSpace
                      betweenBrackets do
                        some' digit
                        char ','
                        some' digit
                      pure Numeric

createTableP :: forall m. (Monad m) => ParserT String m String
createTableP = do (string "create table " <|> string "create table ")
                  word

schemaP :: forall m. (Monad m) => ParserT String m (Array SQLTable)
schemaP = manyMaybe tableP

tableP :: forall m. (Monad m) => ParserT String m SQLTable
tableP = do
  name <- createTableP
  whiteSpace
  string "("
  whiteSpace
  fields <- toUnfoldable <$> (sepBy1 (fieldP name) (optional whiteSpace *> char ',' *> optional whiteSpace *> optional commentP *> optional whiteSpace))
  optional whiteSpace
  string ")"
  optional (string ";")
  pure $ SQLTable {name, fields}

commentP :: forall m. (Monad m) => ParserT String m Unit
commentP = string "--" *> manyTill anyChar (string "\n") *> pure unit

fieldP :: forall m. (Monad m) => String -> ParserT String m SQLField
fieldP table = do
  name <- word
  whiteSpace
  t <- typeP
  optional whiteSpace
  qualifiers <- foldMap toLower <$> sepBy (option "" $ choice [string "primary key", string "PRIMARY KEY", string "not null", string "NOT NULL", string "unique", string "UNIQUE"]) (string " ")
  let primarykey = contains "primary key" qualifiers
  let notnull = contains "not null" qualifiers
  optional whiteSpace
  nt <- optionMaybe (string "/* newtype " *> word >>= (\w -> string " */" *> pure w))
  pure $ SQLField {name, table, "type": t, primarykey, notnull, newtype: nt}

