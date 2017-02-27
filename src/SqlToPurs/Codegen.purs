module SqlToPurs.Codegen where

import Control.Monad.Eff (runPure, Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw, message, catchException)
import Data.Array (length, mapWithIndex, nubBy, range, zip, (..))
import Data.Either (Either(Left, Right))
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
genTypeDeclForGetAll t = genTypeDecl [] (tableToOutMatchedFields t) true ("getAll" <> capitalize (unwrap t).name)

genFuncDefForGetAll :: SQLTable -> String
genFuncDefForGetAll t =
  genFuncDef
    ("getAll" <> capitalize (unwrap t).name)
    (tableToNewtypeName t)
    []
    true
    (genQueryForGetAll t)

genTypeDeclForGetOne :: MatchedInField -> SQLTable -> String
genTypeDeclForGetOne id t =
  genTypeDecl
    [id]
    (tableToOutMatchedFields t)
    false
    ("getOneFrom" <> capitalize (unwrap t).name)

genFuncDefForGetOne :: MatchedInField -> SQLTable -> String
genFuncDefForGetOne id t =
  genFuncDef
    ("getOneFrom" <> capitalize (unwrap t).name)
    (tableToNewtypeName t)
    [id]
    false
    (genQueryForGetOne id t)

genTypeDeclForDelete :: MatchedInField -> SQLTable -> String
genTypeDeclForDelete id t =
  genTypeDecl
    [id]
    (tableToOutMatchedFields t)
    false
    ("deleteFrom" <> capitalize (unwrap t).name)

genFuncDefForDelete :: MatchedInField -> SQLTable -> String
genFuncDefForDelete id t =
  genFuncDef
    ("deleteFrom" <> capitalize (unwrap t).name)
    (tableToNewtypeName t)
    [id]
    false
    (genQueryForDelete id t)

genTypeDeclForUpsert :: SQLTable -> String
genTypeDeclForUpsert t =
  genTypeDecl
    (tableToInMatchedFields t)
    (tableToOutMatchedFields t)
    false
    ("upsert" <> capitalize (unwrap t).name)

genFuncDefForUpsert :: SQLTable -> String
genFuncDefForUpsert t =
  genFuncDef
    ("upsert" <> capitalize (unwrap t).name)
    (tableToNewtypeName t)
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
  let typedecl = genTypeDecl namedFieldsIn' namedFieldsOut' set name
  let funcdef = genFuncDef name recname namedFieldsIn' set (genQueryForFunc name namedFieldsIn')
  pure $ typedecl <> "\n" <> funcdef <> "\n" <> nt <> "\n" <> nti <> "\n" <> forn <> "\n\n"
genFullFunc ts _ s@(SQLFunc {name, vars: {in: invars, out: outvars@(FullTable tableN)}, set, outers}) = do
  t <- maybe (throw $ "couldn't find table " <> tableN) pure (findTable ts tableN)
  let recname = tableToNewtypeName t
  let namedFieldsOut' = tableToOutMatchedFields t
  namedFieldsIn' <- matchInVars ts invars
  let typedecl = genTypeDecl namedFieldsIn' namedFieldsOut' set name
  let funcdef = genFuncDef name recname namedFieldsIn' set (genQueryForFunc name namedFieldsIn')
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

genTypeDecl :: Array MatchedInField -> Array MatchedOutField -> Boolean -> String -> String
genTypeDecl infields outfields set name =
  let outrec = toRecord Nothing outfields
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
  let name = getOutFieldName m
      noNewtypeNoNullable = "readSqlProp \"" <> toLower name <> "\" obj"
      noNewtypeNoNullableWithType tp =  noNewtypeNoNullable <> " :: F " <> tp
      noNewtypeWithNullableWithType tp = noNewtypeNoNullable <> " :: F (Maybe "<> tp <> ")"
      withNewTypeNoNullableWithType = \nts -> nts <> " <$> (" <> noNewtypeNoNullable <> " :: F " <> typeToPurs t <> ")"
      withNewTypeWithNullableWithType = \nts -> "(map " <> nts <> ") <$> (" <> noNewtypeNoNullable <> " :: F (Maybe "<> typeToPurs t <>")" <>  ")" -- To test!
      nullable = not (primarykey || notnull) || oj
   in "(" <> 
      (case nt of 
            NoAnn -> if nullable then noNewtypeWithNullableWithType (annotationToPurs t NoAnn) else noNewtypeNoNullableWithType (annotationToPurs t NoAnn)
            Data d -> if nullable then noNewtypeWithNullableWithType (annotationToPurs t (Data d)) else noNewtypeNoNullableWithType (annotationToPurs t (Data d))
            NewType nts -> if nullable then withNewTypeWithNullableWithType nts else withNewTypeNoNullableWithType nts
        ) <> ")"

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
    name <> " :: " <> (if primarykey || notnull && (not oj) then "" else "Maybe ") <> annotationToPurs t nt
    where
      name = getOutFieldName m

annotationToPurs :: Type -> TypeAnn -> String
annotationToPurs t NoAnn = typeToPurs t
annotationToPurs t (NewType nt) = nt
annotationToPurs t (Data d) = d

typeToPurs :: Type -> String
typeToPurs Int = "Int"
typeToPurs Boolean = "Boolean"
typeToPurs Numeric = "Number"
typeToPurs Text = "String"
typeToPurs UUID = "UUID"
typeToPurs Date = "Date"
typeToPurs TimestampWithoutTimeZone = "DateTime"
typeToPurs Time = "Time"
typeToPurs (PGArray t) = "Array (" <> typeToPurs t <> ")"

genFuncDef :: String -> String -> Array MatchedInField -> Boolean -> String -> String
genFuncDef name outRecName invars set query =
    name <> " cl " <> (if (length invars > 0) then "obj" else "") <> " = (map unwrap) <$> "
    <> (if set then "query " else "queryOne ")
    <> parens ("Query " <> quotes query <> " :: Query " <> outRecName) <> " "
    <> squareBrackets (invars <#> (writeInVar >>> append "toSql ") # joinWith ", ")
    <> " cl"

parens s = "(" <> s <> ")"
squareBrackets s = "[" <> s <> "]"
quotes s = "\"" <> s <> "\""

writeInVar :: MatchedInField -> String
writeInVar m@(MatchedInField v (SQLField {newtype: NoAnn})) = "obj." <> getVarName v
writeInVar m@(MatchedInField v (SQLField {newtype: (NewType _)})) = "(unwrap obj." <> getVarName v <> ")"
writeInVar m@(MatchedInField v (SQLField {newtype: (Data _)})) = "obj." <> getVarName v

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
