module SqlToPurs.Codegen where

import Control.Monad.Eff (runPure, Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw, message, catchException)
import Data.Array (length, mapWithIndex, nubBy, range, zip, (..))
import Data.Either (Either(Left, Right), either)
import Data.Foldable (find, foldMap)
import Data.List (List, elemIndex)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.String (drop, joinWith, take, toLower, toUpper)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, append, bind, flip, id, map, not, pure, show, unit, (#), ($), (&&), (-), (/=), (<#>), (<$>), (<>), (==), (>), (>>>), (||))
import SqlToPurs.Model (OutParams(FullTable, Separate), OuterJoined(OuterJoined), SQLField(..), SQLFunc(SQLFunc), SQLTable(..), Type(PGArray, Time, TimestampWithoutTimeZone, Date, UUID, Text, Numeric, Boolean, Int), TypeAnn(Data, NewType, NoAnn), Var(Var))

-- Model
-- MatchInField = Input parameter of function, that is matched with a field in a table
-- MatchOutField = Output parameter of function, that is matched with a field in a table

type Exc a = Eff (err :: EXCEPTION) a

header :: String -> String
header m = joinWith "\n" [ "module " <> m <> " where"
                         , "import Prelude ((<$>), map, (<*>))"
                         , "import Database.Postgres (Client, DB, query, Query(Query), queryOne)"
                         , "import Control.Monad.Aff (Aff)"
                         , "import Data.Maybe (Maybe)"
                         , "import Data.Foreign (F)"
                         , "import Data.Newtype (class Newtype, unwrap)"
                         , "import Database.Postgres.SqlValue (toSql, readSqlProp, fromSql, class IsSqlValue)" ]

full :: Array SQLTable -> Array SQLFunc -> Exc String
full ts fs = do
  let gennedTableNewtypes =
        ts <#> (\t -> genNewtypeForTable t <> "\n" <>
                      genNewtypeInstanceForTable t <> "\n" <>
                      genForeignForTable t <> "\n")
           # joinWith "\n"
  gennedCrudDefinitions <-
        ts <#> (\t -> do
                 let getalls = genTypeDeclForGetAll t <> "\n" <> genFuncDefForGetAll t <> "\n"
                 let idVar = (Var "id" (unwrap t).name "id")
                 id <- findField ts idVar <#> (MatchedInField idVar)
                 let getones = genTypeDeclForGetOne id t <> "\n" <> genFuncDefForGetOne id t <> "\n"
                 let deletes = genTypeDeclForDelete id t <> "\n" <> genFuncDefForDelete id t <> "\n"
                 let upserts = genTypeDeclForUpsert t <> "\n" <> genFuncDefForUpsert t <> "\n"
                 pure $ joinWith "\n" [getalls, getones, deletes, upserts]
                 )
           # sequence
           <#> joinWith "\n"
  gennedFunctions <- fs `flip mapWithIndex` (genFullFunc ts)
                        # sequence
                        <#> joinWith "\n"
  pure $ joinWith "\n" [
    "-- Table newtypes",
    gennedTableNewtypes,
    "-- CRUD definitions",
    gennedCrudDefinitions,
    "-- PGSQL Function definitions",
    gennedFunctions
    ]

genTypeDeclForGetAll :: SQLTable -> String
genTypeDeclForGetAll t = genTypeDecl [] (Right t) true ("getAll" <> capitalize (unwrap t).name)

genFuncDefForGetAll :: SQLTable -> String
genFuncDefForGetAll t =
  genFuncDef
    ("getAll" <> capitalize (unwrap t).name)
    (Right t)
    []
    true
    (genQueryForGetAll t)

genTypeDeclForGetOne :: MatchedInField -> SQLTable -> String
genTypeDeclForGetOne id t =
  genTypeDecl
    [id]
    (Right t)
    false
    ("getOneFrom" <> capitalize (unwrap t).name)

genFuncDefForGetOne :: MatchedInField -> SQLTable -> String
genFuncDefForGetOne id t =
  genFuncDef
    ("getOneFrom" <> capitalize (unwrap t).name)
    (Right t)
    [id]
    false
    (genQueryForGetOne id t)

genTypeDeclForDelete :: MatchedInField -> SQLTable -> String
genTypeDeclForDelete id t =
  genTypeDecl
    [id]
    (Right t)
    false
    ("deleteFrom" <> capitalize (unwrap t).name)

genFuncDefForDelete :: MatchedInField -> SQLTable -> String
genFuncDefForDelete id t =
  genFuncDef
    ("deleteFrom" <> capitalize (unwrap t).name)
    (Right t)
    [id]
    false
    (genQueryForDelete id t)

genTypeDeclForUpsert :: SQLTable -> String
genTypeDeclForUpsert t =
  genTypeDecl
    (tableToInMatchedFields t)
    (Right t)
    false
    ("upsert" <> capitalize (unwrap t).name)

genFuncDefForUpsert :: SQLTable -> String
genFuncDefForUpsert t =
  genFuncDef
    ("upsert" <> capitalize (unwrap t).name)
    (Right t)
    (tableToInMatchedFields t)
    false
    (genQueryForUpsert t)

genFullFunc :: Array SQLTable -> Int -> SQLFunc -> Exc String
genFullFunc ts i s@(SQLFunc {name, vars: {in: invars, out: outvars@(Separate sepout)}, set, outers})  = do
  let recname = "Res" <> show i
  checkDuplicateVars sepout
  namedFieldsOut' <- matchOutVars ts sepout outers
  namedFieldsIn' <- matchInVars ts invars
  let nt = genNewType namedFieldsOut' recname
  let nti = genNewtypeInstance recname
  let forn = genForeign namedFieldsOut' recname
  let typedecl = genTypeDecl namedFieldsIn' (Left namedFieldsOut') set name
  let funcdef = genFuncDef name (Left recname) namedFieldsIn' set (genQueryForFunc name namedFieldsIn')
  pure $ typedecl <> "\n" <> funcdef <> "\n" <> nt <> "\n" <> nti <> "\n" <> forn <> "\n\n"
genFullFunc ts _ s@(SQLFunc {name, vars: {in: invars, out: outvars@(FullTable tableN)}, set, outers}) = do
  t <- maybe (throw $ "couldn't find table " <> tableN) pure (findTable ts tableN)
  let recname = tableToNewtypeName t
  let namedFieldsOut' = tableToOutMatchedFields t
  namedFieldsIn' <- matchInVars ts invars
  let typedecl = genTypeDecl namedFieldsIn' (Left namedFieldsOut') set name
  let funcdef = genFuncDef name (Left recname) namedFieldsIn' set (genQueryForFunc name namedFieldsIn')
  pure $ typedecl <> "\n" <> funcdef <> "\n\n"

genNewtypeForTable :: SQLTable -> String
genNewtypeForTable t =
  genNewType (tableToOutMatchedFields t) (tableToNewtypeName t)

genNewtypeInstanceForTable :: SQLTable -> String
genNewtypeInstanceForTable t = genNewtypeInstance (tableToNewtypeName t)

genForeignForTable :: SQLTable -> String
genForeignForTable t =
  genForeign (tableToOutMatchedFields t) (tableToNewtypeName t)

toEither :: forall a. Exc a -> Either String a
toEither effA =
  let wrapped = Right <$> effA
   in runPure $ catchException (\e -> pure $ Left $ message e) wrapped

findTable :: Array SQLTable -> String -> Maybe SQLTable
findTable ts tableN = find (\(SQLTable {name}) -> toLower name == toLower tableN) ts

tableToOutMatchedFields :: SQLTable -> Array MatchedOutField
tableToOutMatchedFields (SQLTable {fields}) =
  (\f -> MatchedOutField Nothing f (OuterJoined false)) <$> fields

tableToInMatchedFields :: SQLTable -> Array MatchedInField
tableToInMatchedFields (SQLTable {fields}) =
  (\(SQLField f) -> MatchedInField (Var f.name f.table f.name) (SQLField f)) <$> fields

checkDuplicateVars :: Array Var -> Exc Unit
checkDuplicateVars vs =
  let totalLength = length vs
      nubbedLength = length $ nubBy (\v1 v2 -> getVarName v1 == getVarName v2) vs
   in if totalLength /= nubbedLength then (throw "Duplicate fields, not supported")
                                     else pure unit

genTypeDecl :: Array MatchedInField -> Either (Array MatchedOutField) SQLTable -> Boolean -> String -> String
genTypeDecl infields out set name =
  let outrec = either (toRecord Nothing) (tableToNewtypeName) out
      inrec = toRecord (Just "obj") infields
   in name
         <> " :: forall eff " <> (if length infields > 0 then "obj" else "") <>". Client -> "
         <> inrec
         <> (if length infields > 0 then " -> " else "")
         <> "Aff (db :: DB | eff) "
         <> "(" <> (if set then "Array " else "Maybe ") <> outrec <> ")" -- queryOne returns Maybe

tableToNewtypeName :: SQLTable -> String
tableToNewtypeName (SQLTable t) = capitalize t.name <> "Rec"

genNewType :: Array MatchedOutField -> String -> String
genNewType nf nm =
  "newtype " <> nm <> " = " <> nm <> " " <> toRecord Nothing nf

genNewtypeInstance :: String -> String
genNewtypeInstance nm = "derive instance newtype" <> nm <> " :: Newtype " <> nm <> " _ "

genForeign :: Array MatchedOutField -> String -> String
genForeign fields nm =
  let objSugar =  "{" <> joinWith ", " (map (\m -> getOutFieldName m <> ": _") fields) <> "}"
  in  "instance isSqlValue" <> nm <> " :: IsSqlValue " <> nm <>" where "
        <> "\n" <> " toSql a = toSql \"\""
        <> "\n" <> " fromSql obj = " <> nm <> " <$> " <> "(" <> objSugar <> " <$> " <> (joinWith " <*> " (map genReadProp fields)) <> ")"

genReadProp :: MatchedOutField -> String
genReadProp m@(MatchedOutField _ (SQLField {primarykey, notnull, type: t, newtype: nt}) (OuterJoined oj))=
  parens (readStatement <> " :: F " <> expectedTypeAnnotation)
  where
    nullable = not (primarykey || notnull) || oj
    readStatement = "readSqlProp " <> quotes (toLower (getOutFieldName m)) <> " obj"
    expectedTypeAnnotation = if nullable then parens ("Maybe " <> (annotationToPurs t nt)) else annotationToPurs t nt

matchOutVars :: Array SQLTable -> Array Var -> Maybe (List String) -> Exc (Array MatchedOutField)
matchOutVars ts vars outers = flip traverse vars go
  where
  outers' = maybe mempty id outers
  go v = do
    foundField <- findField ts v
    let outerjoined = OuterJoined $ isJust (elemIndex (toLower (unwrap foundField).table) outers')
    pure $ MatchedOutField (Just v) foundField outerjoined

matchInVars :: Array SQLTable -> Array Var -> Exc (Array MatchedInField)
matchInVars ts vars = flip traverse vars go
  where
  go v = do
    foundField <- findField ts v
    pure $ MatchedInField v foundField


findField :: Array SQLTable -> Var -> Exc SQLField
findField ts v@(Var name tableN fieldN) = do
    foundTable <- maybe
                    (throw $ "Table not found: " <> tableN)
                    pure
                    (find (\(SQLTable {name}) -> toLower name == toLower tableN) ts)
    maybe
      (throw $ "Field not found: " <> tableN)
      pure
      (find (\(SQLField {name}) -> toLower name == toLower fieldN) (unwrap foundTable).fields)

toRecord :: forall a. MatchedField a => Maybe String -> Array a -> String
toRecord _   [] = ""
toRecord ext fs = "{" <> joinWith ", " (toPurs <$> fs) <> (maybe "" (\obj -> " | " <> obj) ext) <> "}"

-- I made this into two seperate types and a class because I don't want In and Out fields to be allowed in the same Array
data MatchedInField = MatchedInField Var SQLField
data MatchedOutField = MatchedOutField (Maybe Var) SQLField OuterJoined
class MatchedField a where
  toPurs :: a -> String

instance matchedFieldIn :: MatchedField MatchedInField where
  toPurs (MatchedInField (Var name _ _) (SQLField {type: t, primarykey, notnull, newtype: nt})) =
    name <> " :: " <> (if primarykey || notnull then "" else "Maybe ") <> annotationToPurs t nt

instance matchedFieldOut :: MatchedField MatchedOutField where
  toPurs m@(MatchedOutField v (SQLField {name: n, type: t, primarykey, notnull, newtype: nt}) (OuterJoined oj)) =
    name <> " :: " <> (if primarykey || notnull && (not oj) then annotationToPurs t nt else "Maybe " <> (annotationToPurs t nt))
    where
      name = getOutFieldName m

annotationToPurs :: Type -> TypeAnn -> String
annotationToPurs (PGArray t) nt = parens ("Array " <> (annotationToPurs t nt))
annotationToPurs t (NewType nt) = nt
annotationToPurs t (Data d) = d
annotationToPurs Int _ = "Int"
annotationToPurs Boolean _ = "Boolean"
annotationToPurs Numeric _ = "Number"
annotationToPurs Text _ = "String"
annotationToPurs UUID _ = "UUID"
annotationToPurs Date _ = "Date"
annotationToPurs TimestampWithoutTimeZone _ = "DateTime"
annotationToPurs Time _ = "Time"

genFuncDef :: String -> (Either String SQLTable) -> Array MatchedInField -> Boolean -> String -> String
genFuncDef name out invars set query =
    name <> " cl " <> (if (length invars > 0) then "obj" else "") <> " = " <> either (\_ -> "(map unwrap) <$> ") (\_ -> "") out
    <> (if set then "query " else "queryOne ")
    <> parens ("Query " <> quotes query <> " :: Query " <> (either id tableToNewtypeName out)) <> " "
    <> squareBrackets (invars <#> (writeInVar >>> append "toSql ") # joinWith ", ")
    <> " cl"

parens s = "(" <> s <> ")"
squareBrackets s = "[" <> s <> "]"
quotes s = "\"" <> s <> "\""

writeInVar :: MatchedInField -> String
writeInVar (MatchedInField v _) = "obj." <> getVarName v

-- genQueryType outRecName = if set then "Query (Array " <> outRecName <> ")" else "Query " <> outRecName

genQueryForFunc :: String -> Array MatchedInField -> String
genQueryForFunc name invars = "select * from " <> name <> parens (toQuestionmarks invars)

genQueryForGetAll :: SQLTable -> String
genQueryForGetAll t = "select * from " <> (unwrap t).name

genQueryForGetOne :: MatchedInField -> SQLTable -> String
genQueryForGetOne (MatchedInField v _) t = "select * from " <> (unwrap t).name <> " where " <> getVarName v <> " = $1"

genQueryForDelete :: MatchedInField -> SQLTable -> String
genQueryForDelete (MatchedInField v _) t = "delete from " <> (unwrap t).name <> " where " <> getVarName v <> " = $1 RETURNING *"

genQueryForUpsert :: SQLTable -> String
genQueryForUpsert t = "insert into " <> (unwrap t).name <> " values " <> parens (toQuestionmarks (tableToInMatchedFields t)) <> " ON CONFLICT DO UPDATE RETURNING *"

getVarName :: Var -> String
getVarName (Var n _ _) = n

getOutFieldName :: MatchedOutField -> String
getOutFieldName (MatchedOutField v (SQLField {name}) _) = maybe name getVarName v

toQuestionmarks :: forall a. Array a -> String
toQuestionmarks [] = ""
toQuestionmarks as = joinWith "," $ map (\i -> "$" <> show i) (1 .. (length as))

capitalize :: String -> String
capitalize s = toUpper (take 1 s) <> drop 1 s
