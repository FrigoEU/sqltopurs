module SqlToPurs.Codegen where

import Control.Monad.Eff (runPure, Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw, message, catchException)
import Data.Array (length, nubBy, range, zip, (..))
import Data.Either (Either(Left, Right))
import Data.Foldable (find, foldMap)
import Data.List (List, elemIndex)
import Data.Maybe (Maybe(Nothing, Just), isJust, maybe)
import Data.Monoid (mempty)
import Data.String (joinWith, toLower)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, bind, flip, id, map, not, pure, show, unit, ($), (&&), (-), (/=), (<#>), (<$>), (<>), (==), (>), (>>=), (||))
import SqlToPurs.Model (NamedField(..), OutParams(..), OuterJoined(..), SQLField(SQLField), SQLFunc(SQLFunc), SQLTable(SQLTable), Type(..), TypeAnn(Data, NewType, NoAnn), Var(Var))

type Exc a = Eff (err :: EXCEPTION) a

header :: String -> String
header m = joinWith "\n" [ "module " <> m <> " where"
                         , "import Prelude ((<$>), map, (<*>))"
                         , "import Database.Postgres (Client, DB, query, Query(Query), queryOne)"
                         , "import Control.Monad.Aff (Aff)"
                         , "import Data.Maybe (Maybe)"
                         , "import Data.Foreign (F)"
                         , "import Database.Postgres.SqlValue (toSql, readSqlProp, fromSql, class IsSqlValue)" ]

full :: Array SQLTable -> Array SQLFunc -> Either String String
full ts fs = let withIndex = zip fs (range 0 (length fs - 1))
                 lines = flip map withIndex (\(Tuple s@(SQLFunc {name, vars: {in: invars, out: outvars}, set, outers}) i) -> 
                                                let recname = "Res" <> show i
                                                 in do 
                                                   checkDuplicateVars outvars
                                                   nt <- genNewType recname ts outvars outers
                                                   run <- genRun recname ts outvars outers
                                                   forn <- genForeign recname ts outvars outers
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

checkDuplicateVars :: OutParams -> Exc Unit
checkDuplicateVars (FullTable _) = pure unit
checkDuplicateVars (Separate vs) =
  let totalLength = length vs
      nubbedLength = length $ nubBy (\v1 v2 -> getVarName v1 == getVarName v2) vs
   in if totalLength /= nubbedLength then (throw "Duplicate fields, not supported")
                                     else pure unit

genTypeDecl :: Array SQLTable -> SQLFunc -> Exc String
genTypeDecl ts (SQLFunc {name, vars: {in: invars, out: outvars}, set, outers}) = do
  outrec <- outParamsToRecord ts outvars outers
  infields <- varsToNamedFields ts invars outers
  pure $ name
         <> " :: forall eff " <> (if length infields > 0 then "obj" else "") <>". Client -> " 
         <> namedFieldsToRecord (Just "obj") (infields <#> \f -> Tuple f (OuterJoined false))
         <> (if length infields > 0 then " -> " else "")
         <> "Aff (db :: DB | eff) "
         <> "(" <> (if set then "Array " else "Maybe ") <> outrec <> ")" -- queryOne returns Maybe

genNewType :: String -> Array SQLTable -> OutParams -> Maybe (List String) -> Exc String
genNewType nm ts outp outers = outParamsToRecord ts outp outers >>= \record -> pure $ "newtype " <> nm <> " = " <> nm <> " " <> record

genRun :: String -> Array SQLTable -> OutParams -> Maybe (List String) -> Exc String
genRun nm ts outp outers = outParamsToRecord ts outp outers >>= \record -> pure $ "run" <> nm <> " :: " <> nm <> " -> " <> record <> "\n" <> "run" <> nm <> " (" <> nm <> " a) = a"

genForeign :: String -> Array SQLTable -> OutParams -> Maybe (List String) -> Exc String
genForeign nm ts outp outers = do
  fields <- outParamsToNamedFields ts outp outers
  let objSugar =  "{" <> joinWith ", " (map (\(Tuple f _) -> getFieldName f <> ": _") fields) <> "}"
  pure $ "instance isSqlValue" <> nm <> " :: IsSqlValue " <> nm <>" where " 
          <> "\n" <> " toSql a = toSql \"\"" 
          <> "\n" <> " fromSql obj = " <> nm <> " <$> " <> "(" <> objSugar <> " <$> " <> (joinWith " <*> " (map genReadProp fields)) <> ")"

genReadProp :: Tuple NamedField OuterJoined -> String
genReadProp (Tuple nf@(NamedField {field: (SQLField {primarykey, notnull, type: t, newtype: nt})}) (OuterJoined oj))=
  let name = getFieldName nf
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

outParamsToNamedFields :: Array SQLTable -> OutParams -> Maybe (List String) -> Exc (Array (Tuple NamedField OuterJoined))
outParamsToNamedFields ts (FullTable tableN) _ = maybe (throw $ "Table " <> tableN <> "not found!") pure (tableToNamedFields ts tableN <#> map (\f -> Tuple f (OuterJoined false)))
outParamsToNamedFields ts (Separate vars) outers =
  varsToNamedFields ts vars outers
  >>= \nfs -> pure $ nfs <#> (\(nf@(NamedField {field: SQLField {table}})) -> Tuple nf (OuterJoined $ isJust (elemIndex (toLower table) outers')))
  where outers' = maybe mempty id outers

varsToNamedFields :: Array SQLTable -> Array Var -> Maybe (List String) -> Exc (Array NamedField)
varsToNamedFields ts vars outs = traverse (\v -> maybe (throw $ show v <> "not found!") pure $ varToNamedField ts v) vars

outParamsToRecord :: Array SQLTable -> OutParams -> Maybe (List String) -> Exc String
outParamsToRecord ts outp outers = namedFieldsToRecord Nothing <$> (outParamsToNamedFields ts outp outers)

namedFieldsToRecord :: Maybe String -> Array (Tuple NamedField OuterJoined) -> String
namedFieldsToRecord _   [] = ""
namedFieldsToRecord ext fs = "{" <> joinWith ", " (namedFieldToPurs <$> fs) <> (maybe "" (\obj -> " | " <> obj) ext) <> "}" 

namedFieldToPurs :: Tuple NamedField OuterJoined -> String
namedFieldToPurs (Tuple nf@(NamedField {field: (SQLField {type: t, primarykey, notnull, newtype: nt})}) (OuterJoined oj)) = 
  name <> " :: " <> (if primarykey || notnull && (not oj) then "" else "Maybe ") <> annotationToPurs t nt
    where
      name = getFieldName nf

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
      <> "[" <> joinWith ", " ((\v -> "toSql " <> getVarName v) <$> invars) <> "]"
      <> " cl"
        where
          writeInVar :: Var -> String
          writeInVar v = let nf  = varToNamedField ts v
                             invarname = getVarName v
                          in case nf of
                                  Nothing -> ""
                                  (Just (NamedField {field: (SQLField {newtype: NoAnn})})) -> invarname
                                  (Just (NamedField {field: (SQLField {newtype: (NewType nts)})})) -> invarname <> ": " <> "(" <> nts <> " " <>  invarname <> ")"
                                  (Just (NamedField {field: (SQLField {newtype: (Data d)})})) -> invarname

getVarName :: Var -> String
getVarName (Var (Just n) _     _    ) = n
getVarName (Var Nothing  table field) = table <> "_" <> field



getFieldName :: NamedField -> String
getFieldName (NamedField {name: n, field: (SQLField {name})}) = maybe name id n


toQuestionmarks :: forall a. Array a -> String
toQuestionmarks [] = ""
toQuestionmarks as = joinWith "," $ map (\i -> "$" <> show i) (1 .. (length as))
