module SqlToPurs.Codegen where

import Control.Monad.Eff (runPure, Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw, message, catchException)
import Data.Array (range, zip, length, replicate)
import Data.Either (Either(Left, Right))
import Data.Foldable (find, foldMap)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.String (toLower, joinWith)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(Tuple))
import Prelude (not, ($), id, (<>), (<$>), (>), (||), pure, show, map, bind, (>>=), (==), flip, (-))
import SqlToPurs.Model (NamedField(NamedField), OutParams(Separate, FullTable), SQLField(SQLField), SQLTable(SQLTable), Var(Var), SQLFunc(SQLFunc), Type(TimestampWithTimeZone, TimestampWithoutTimeZone, SqlDate, UUID, Text, Numeric, Boolean, Int))

type Exc a = Eff (err :: EXCEPTION) a

header :: String
header = joinWith "\n" [ "module MyApp.SQL where"
                       , "import Prelude ((<$>), return, bind, map, ($), (<*>), (>>=))"
                       , "import Database.Postgres (Client, DB, query, Query(Query), queryOne)"
                       , "import Control.Monad.Aff (Aff)"
                       , "import Data.Maybe (Maybe)"
                       , "import Data.Foreign (F)"
                       , "import Data.Foreign.Null (Null, runNull)"
                       , "import Database.Postgres.SqlValue (toSql)"
                       , "import Data.Foreign.Class (class IsForeign, readProp, read)"]

full :: Array SQLTable -> Array SQLFunc -> Either String String
full ts fs = let withIndex = zip fs (range 0 (length fs - 1))
                 lines = flip map withIndex (\(Tuple s@(SQLFunc {name, vars: {in: invars, out: outvars}, set}) i) -> 
                                                let recname = "Res" <> show i
                                                 in do 
                                                   nt <- genNewType recname ts outvars
                                                   run <- genRun recname ts outvars
                                                   forn <- genForeign recname ts outvars
                                                   typedecl <- genTypeDecl ts s
                                                   let funcdef = genFuncDef ts recname s
                                                   pure $ typedecl <> "\n" <> funcdef <> "\n" <> nt <> "\n" <> run <> "\n" <> forn <> "\n\n" ) 
                 line = (foldMap id <$> sequence lines) :: Exc String
              in toEither line

toEither :: forall a. Exc a -> Either String a
toEither effA =
  let wrapped = Right <$> effA
   in runPure $ catchException (\e -> pure $ Left $ message e) wrapped

tableToNamedFields :: Array SQLTable -> String -> Maybe (Array NamedField)
tableToNamedFields ts tableN = find (\(SQLTable {name}) -> toLower name == toLower tableN) ts >>= \(SQLTable {fields}) -> pure $ (\f -> NamedField {name: Nothing, field: f}) <$> fields

varToNamedField :: Array SQLTable -> Var -> Maybe NamedField
varToNamedField ts (Var n tableN fieldN) = do
  fields <- tableToNamedFields ts tableN
  (NamedField {field}) <- find (\(NamedField {field: (SQLField {name})}) -> toLower name == toLower fieldN ) fields
  pure $ NamedField {name: n, field: field}


genTypeDecl :: Array SQLTable -> SQLFunc -> Exc String
genTypeDecl ts (SQLFunc {name, vars: {in: invars, out: outvars}, set}) = do
  outrec <- outParamsToRecord ts outvars
  infields <- varsToNamedFields ts invars
  pure $ name 
         <> " :: forall eff. Client -> " 
         <> namedFieldsToRecord infields
         <> (if (length infields > 0) then " -> " else "")
         <> "Aff (db :: DB | eff) "
         <> "(" <> (if set then "Array " else "Maybe ") <> outrec <> ")" -- queryOne returns Maybe

genNewType :: String -> Array SQLTable -> OutParams -> Exc String
genNewType nm ts outp = outParamsToRecord ts outp >>= \record -> pure $ "newtype " <> nm <> " = " <> nm <> " " <> record

genRun :: String -> Array SQLTable -> OutParams -> Exc String
genRun nm ts outp = outParamsToRecord ts outp >>= \record -> pure $ "run" <> nm <> " :: " <> nm <> " -> " <> record <> "\n" <> "run" <> nm <> " (" <> nm <> " a) = a"

genForeign :: String -> Array SQLTable -> OutParams -> Exc String
genForeign nm ts outp = do
  fields <- outParamsToNamedFields ts outp
  let objSugar =  "{" <> joinWith ", " (map (\f -> getFieldName f <> ": _") fields) <> "}"
  pure $ "instance isForeign" <> nm <> " :: IsForeign " <> nm <>" where read obj = " 
          <> nm <> " <$> " <> "(" <> objSugar <> " <$> " <> (joinWith " <*> " (map genReadProp fields)) <> ")"

genReadProp :: NamedField -> String
genReadProp nf@(NamedField {field: (SQLField {primarykey, notnull, type: t, newtype: nt})}) = 
  let name = getFieldName nf
      noNewtypeNoNullable = "readProp \"" <> name <> "\" obj"
      noNewtypeNoNullableWithType =  noNewtypeNoNullable <> " :: F " <> typeToPurs t
      noNewtypeWithNullableWithType = "runNull <$> (" <> noNewtypeNoNullable <> " :: F (Null "<> typeToPurs t <>")" <>  ")"
      withNewTypeNoNullableWithType = \nts -> nts <> " <$> (" <> noNewtypeNoNullable <> " :: F " <> typeToPurs t <> ")"
      withNewTypeWithNullableWithType = \nts -> "((map " <> nts <> ") <<< runNull) <$> (" <> noNewtypeNoNullable <> " :: F (Null "<> typeToPurs t <>")" <>  ")" -- To test!
      nullable = not (primarykey || notnull)
   in "(" <> 
      (maybe 
        (if nullable then noNewtypeWithNullableWithType else noNewtypeNoNullableWithType)
        (\nts -> if nullable then withNewTypeWithNullableWithType nts else withNewTypeNoNullableWithType nts)
        nt) <>
      ")"

outParamsToNamedFields :: Array SQLTable -> OutParams -> Exc (Array NamedField)
outParamsToNamedFields ts (FullTable tableN) = maybe (throw $ "Table " <> tableN <> "not found!") pure (tableToNamedFields ts tableN)
outParamsToNamedFields ts (Separate vars) = varsToNamedFields ts vars

varsToNamedFields :: Array SQLTable -> Array Var -> Exc (Array NamedField)
varsToNamedFields ts vars = traverse (\v -> maybe (throw $ show v <> "not found!") pure $ varToNamedField ts v) vars

outParamsToRecord :: Array SQLTable -> OutParams -> Exc String
outParamsToRecord ts outp = namedFieldsToRecord <$> (outParamsToNamedFields ts outp)

namedFieldsToRecord :: Array NamedField -> String
namedFieldsToRecord [] = ""
namedFieldsToRecord fs = "{" <> joinWith ", " (namedFieldToPurs <$> fs) <> "}" 

namedFieldToPurs :: NamedField -> String
namedFieldToPurs nf@(NamedField {field: (SQLField {type: t, primarykey, notnull, newtype: nt})}) = 
  name <> " :: " <> (if primarykey || notnull then "" else "Maybe ") <> (maybe (typeToPurs t) id nt)
    where
      name = getFieldName nf

typeToPurs :: Type -> String
typeToPurs Int = "Int"
typeToPurs Boolean = "Boolean"
typeToPurs Numeric = "Number"
typeToPurs Text = "String"
typeToPurs UUID = "UUID"
typeToPurs SqlDate = "SqlDate"
typeToPurs TimestampWithoutTimeZone = "TimestampWithoutTimeZone"
typeToPurs TimestampWithTimeZone = "TimestampWithTimeZone"


genFuncDef :: Array SQLTable -> String -> SQLFunc -> String
genFuncDef ts nm (SQLFunc {name, vars: {in: invars}, set}) =
    name 
      <> " cl "
      <> (if (length invars > 0) 
             then "{" <> (joinWith ", " (writeInVar <$> invars)) <> "}" 
             else "")
      <> " = "
      <> "(map run" <> nm <> ") <$> " 
      <> (if set then "query " else "queryOne ")
      <> "(Query \"select * from " <> name <> "(" <> toQuestionmarks invars <> ")\") "
      <> "[" <> joinWith ", " ((\v -> "toSql " <> getInVarName v) <$> invars) <> "]"
      <> " cl"
        where
          writeInVar :: Var -> String
          writeInVar v = let nf  = varToNamedField ts v
                             invarname = getInVarName v
                          in case nf of
                                  Nothing -> ""
                                  (Just (NamedField {field: (SQLField {newtype: nt})})) -> maybe invarname (\nts -> (invarname <> ": " <> "(" <> nts <> " " <>  invarname <> ")")) nt
          getInVarName :: Var -> String
          getInVarName (Var (Just n) _     _    ) = n
          getInVarName (Var Nothing  table field) = table <> "_" <> field

getFieldName :: NamedField -> String
getFieldName (NamedField {name: n, field: (SQLField {name})}) = maybe name id n


toQuestionmarks :: forall a. Array a -> String
toQuestionmarks as = joinWith "," $ replicate (length as) "?"
