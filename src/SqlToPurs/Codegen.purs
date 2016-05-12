module SqlToPurs.Codegen where

import Data.List as L
import Data.Array (replicate)
import Data.Foldable (foldMap)
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Prelude (($), (<>), (<$>), (>), not, map, (>>>), show, (-))
import SqlToPurs.Model (SQLFunc(SQLFunc), Type(TimestampWithoutTimeZone, SqlDate, UUID, Text, Numeric, Boolean, Int), Var(Out, In))

data NamedRecord = NamedRecord String (L.List Var)

header :: String
header = joinWith "\n" [ "module MyApp.SQL where"
                       , "import Prelude ((<$>), return, bind, map, ($))"
                       , "import Database.AnyDB (Connection, DB, query, Query(Query))"
                       , "import Control.Monad.Aff (Aff)"
                       , "import Data.Foreign.Class (class IsForeign, readProp)"]

full :: L.List SQLFunc -> String
full fs = let withIndex = (L.zip fs (L.range 0 (L.length fs - 1)))
           in foldMap (\(Tuple s i) -> let recname = "Res" <> show i
                                           r = makeOutputRec recname s 
                                        in genNewType r <> "\n"
                                           <> genRun r <> "\n"
                                           <> genForeign r <> "\n"
                                           <> genTypeDecl s <> "\n" 
                                           <> genFuncDef recname s <> "\n\n") 
                                       withIndex

makeOutputRec :: String -> SQLFunc -> NamedRecord
makeOutputRec s (SQLFunc {vars}) = NamedRecord s (L.filter (not isIn) vars)


genTypeDecl :: SQLFunc -> String
genTypeDecl (SQLFunc {name, vars, set}) = 
  let invars = L.filter isIn vars
      outvars = L.filter (not isIn) vars
   in name 
      <> " :: forall eff. Connection -> " 
      <> varsToRecord invars
      <> (if (L.length invars > 0) then " -> " else "")
      <> "Aff (db :: DB | eff) "
      <> "(" <> (if set then "Array " else "") <> varsToRecord outvars <> ")"

isIn :: Var -> Boolean
isIn (In _ _) = true
isIn _        = false

genNewType :: NamedRecord -> String
genNewType (NamedRecord nm vars) = "newtype " <> nm <> " = " <> nm <> " " <> varsToRecord vars

genRun :: NamedRecord -> String
genRun (NamedRecord nm vars) = "run" <> nm <> " :: " <> nm <> " -> " <> varsToRecord vars <> "\n" 
                            <> "run" <> nm <> " (" <> nm <> " a) = a"

genForeign :: NamedRecord -> String
genForeign (NamedRecord nm vars) = "instance isForeign" <> nm <> " :: IsForeign " <> nm <>" where\n"
                                <> "  read obj = do\n"
                                <> foldMap (genReadProp >>> (\s -> "    " <> s <> "\n")) vars 
                                <> "    return $ " <> nm 
                                    <> " {" <> joinWith "," (L.toUnfoldable $ map getName vars) <> "}"

genReadProp :: Var -> String
genReadProp (Out nm UUID) = nm <> " <- UUID <$> readProp \""<> nm <> "\" obj"
genReadProp (Out nm _   ) = nm <> " <- readProp \""<> nm <> "\" obj"
genReadProp (In _ _ ) = "Can't get a readprop from an out variable, system error"

varsToRecord :: L.List Var -> String
varsToRecord L.Nil = ""
varsToRecord vs = "{" <> joinWith ", " (L.toUnfoldable $ varToPurs <$> vs) <> "}" 

varToPurs :: Var -> String
varToPurs (In name t)  = name <> " :: " <> typeToPurs t
varToPurs (Out name t) = name <> " :: " <> typeToPurs t

typeToPurs :: Type -> String
typeToPurs Int = "Int"
typeToPurs Boolean = "Boolean"
typeToPurs Numeric = "Number"
typeToPurs Text = "String"
typeToPurs UUID = "UUID"
typeToPurs SqlDate = "SqlDate"
typeToPurs TimestampWithoutTimeZone = "TimestampWithoutTimeZone"


genFuncDef :: String -> SQLFunc -> String
genFuncDef outRecName (SQLFunc {name, vars, set}) = 
  let invars = L.filter isIn vars
      outvars = L.filter (not isIn) vars
   in name 
      <> " conn "
      <> (if (L.length invars > 0) 
             then "{" <> (joinWith ", " (L.toUnfoldable $ getName <$> invars)) <> "}" 
             else "")
      <> " = "
      <> "(map run" <> outRecName <> ") <$> " 
      <> "query (Query \"select * from " <> name <> "(" <> toQuestionmarks invars <> ")\") "
      <> "[" <> joinWith ", " (L.toUnfoldable $ (\v -> "toSql " <> getName v) <$> invars) <> "]"
      <> " conn"

getName :: Var -> String
getName (In name _) = name
getName (Out name _) = name

toQuestionmarks :: forall a. L.List a -> String
toQuestionmarks as = joinWith "," $ replicate (L.length as) "?"


{-- myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number}) --}
{-- myfunc conn {myinvar} = query "select * from myfunc(?)" [toSql myinvar] conn --}
