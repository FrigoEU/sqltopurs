module SqlToPurs.Parsing where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (class MonadError)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, filter, length, many, snoc, some)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, foldl)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Monoid (mempty)
import Data.String (Pattern(..), contains, fromCharArray, singleton, toLower)
import Data.Tuple (Tuple(Tuple))
import Debug.Trace (spy)
import Prelude (class Monad, Unit, bind, const, map, not, pure, unit, ($), (&&), (/=), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=), (>>>), discard)
import SqlToPurs.Model (OutParams(FullTable, Separate), SQLField(SQLField), SQLFunc(SQLFunc), SQLTable(SQLTable), Type(..), TypeAnn(NewType, Data, NoAnn), Var(Var))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (between, choice, manyTill, option, optionMaybe, optional, sepBy, sepBy1, try, (<?>))
import Text.Parsing.Parser.String (anyChar, char, oneOf, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum, letter, space)

data Dir = In | Out

some' :: forall m. (Monad m) => ParserT String m Char -> ParserT String m String
some' p = fromCharArray <$> some p

dirP :: forall m. (Monad m) => ParserT String m Dir
dirP = (string "IN" >>= \_ -> pure In)
      <|> (string "OUT" >>= \_ -> pure Out)
      <?> "IN or OUT"

word :: forall m. (Monad m) => ParserT String m String
word = some' (alphaNum <|> char '_') <?> "Variable Name"

betweenBrackets :: forall a m. (Monad m) => ParserT String m a -> ParserT String m a
betweenBrackets = between (string "(" *> optional whiteSpace) (optional whiteSpace *> string ")")

varP :: forall m. (Monad m) => ParserT String m Var
varP = do name <- word
          _ <- whiteSpace
          tablename <- word
          _ <- char '.'
          fieldname <- word
          _ <- string "%TYPE"
          pure $ Var name tablename fieldname

varAndDirP :: forall m. (Monad m) => ParserT String m (Tuple Dir Var)
varAndDirP = do
  dir <- dirP
  _ <- whiteSpace
  var <- varP
  pure $ Tuple dir var

varsP :: forall m. (Monad m) => ParserT String m (Array (Tuple Dir Var))
varsP = betweenBrackets $ toUnfoldable <$> (sepBy varAndDirP (optional whiteSpace *> char ',' *> optional whiteSpace))

returnsP :: forall m. (Monad m) => ParserT String m (Maybe (Tuple Boolean String))
returnsP = optionMaybe
       ( do _ <- string "RETURNS SETOF " <|> string "returns setof "
            str <- word
            pure (Tuple true str)
         <|>
         do _ <- string "RETURNS " <|> string "returns "
            str <- word
            pure (Tuple false str))

outersP :: forall m. (Monad m) => ParserT String m (Maybe (List String))
outersP = optionMaybe
          (do _ <- string "-- outer join "
              o <- sepBy1 word (optional whiteSpace *> char ',' *> optional whiteSpace)
              _ <- char ';'
              pure o
          )

createFunctionP :: forall m. (Monad m) => ParserT String m String
createFunctionP = (string "CREATE FUNCTION "
                   <|> string "create function "
                   <|> string "CREATE OR REPLACE FUNCTION "
                   <|> string "create or replace function ")

functionP :: forall m. (Monad m) => (MonadError String m) => ParserT String m SQLFunc
functionP = do
  _ <- createFunctionP
  name <- word
  optional whiteSpace
  vars <- varsP
  optional whiteSpace
  outers <- outersP
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
            then lift $ throwError $ "Function " <> name <> " is not returning anything, not supported. If deleting, just pure the id"
            else pure $ maybe (Separate outvars) FullTable returnsFullTable 
  pure $ SQLFunc {name, vars: {in: invars, out}, set, outers}
    where 
      isIn :: Tuple Dir Var -> Boolean
      isIn (Tuple In v) = true
      isIn _            = false
      runVar :: Tuple Dir Var -> Var
      runVar (Tuple _ v) = v


functionsP :: forall m. (Monad m) => (MonadError String m) => ParserT String m (Array SQLFunc)
functionsP = manyMaybe functionP

manyMaybe :: forall m a. (Monad m) => ParserT String m a -> ParserT String m (Array a)
manyMaybe p = foldl f [] <$> many (maybeP p)
  where
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
        <|> (string "date" >>= \_ -> pure Date)
        <|> (string "timestamp without time zone" >>= \_ -> pure TimestampWithoutTimeZone)
        <|> (string "time" >>= \_ -> pure Time)
        <|> numericP
        <?> "int, boolean, text, uuid, numeric(x,x), date or timestamp without time zone"
  where numericP = do _ <- string "numeric"
                      optional whiteSpace
                      _ <- betweenBrackets do
                        _ <- some' digit
                        _ <- char ','
                        some' digit
                      pure Numeric

createTableP :: forall m. (Monad m) => ParserT String m String
createTableP = do _ <- string "create table " <|> string "create table "
                  word

schemaP :: forall m. (Monad m) => ParserT String m (Array SQLTable)
schemaP = manyMaybe tableP

tableP :: forall m. (Monad m) => ParserT String m SQLTable
tableP = do
  name <- createTableP
  _ <- whiteSpace
  _ <- string "("
  _ <- whiteSpace
  fields <- catMaybes <$> toUnfoldable <$> (
    sepBy1 (choice [(const Nothing) <$> foreignKey, Just <$> (fieldP name)])
           (optional whiteSpace *> char ',' *>
            optional whiteSpace *> optional commentP *> optional whiteSpace))
  optional whiteSpace
  string ")" *> optional (string ";")
  pure $ SQLTable {name, fields}

commentP :: forall m. (Monad m) => ParserT String m Unit
commentP = string "--" *> manyTill anyChar (string "\n") *> pure unit

fieldP :: forall m. (Monad m) => String -> ParserT String m SQLField
fieldP table = do
  name <- word
  _ <- whiteSpace
  t' <- typeP
  t <- optionMaybe (string "[]") <#> maybe t' (\_ -> PGArray t')
  optional whiteSpace
  qualifiers <- foldMap toLower <$> sepBy (option "" $ choice [string "primary key", string "PRIMARY KEY", string "not null", string "NOT NULL", string "unique", string "UNIQUE"]) (string " ")
  let primarykey = contains (Pattern "primary key") qualifiers
  let notnull = contains (Pattern "not null") qualifiers
  optional whiteSpace
  nt <- annotationP
  optional whiteSpace
  pure $ SQLField {name, table, "type": t, primarykey, notnull, newtype: nt}

annotationP = (string "/* newtype " *> (manyTill charsForAnnotation (string " */") <#> (catChars >>> NewType)))
              <|> (string "/* data " *> (manyTill charsForAnnotation (string " */") <#> (catChars >>> NewType)))
              <|> pure NoAnn

charsForAnnotation = alphaNum <|> char '_' <|> char '(' <|> char ')' <|> char ' '

foreignKey :: forall m. (Monad m) => ParserT String m Unit
foreignKey = do
  _ <- string "FOREIGN KEY" <|> string "foreign key"
  _ <- whiteSpace
  _ <- char '(' *> manyTill (letter <|> space <|> char ',') (char ')')
  _ <- whiteSpace
  _ <- string "REFERENCES" <|> string "references"
  _ <- whiteSpace
  _ <- word
  bla <- char '(' *> manyTill (letter <|> space <|> char ',') (char ')')
  optional whiteSpace
  pure unit

catChars :: List Char -> String
catChars = fold <<< map singleton
