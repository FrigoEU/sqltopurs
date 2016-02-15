module SqlToPurs.Codegen where

import Prelude (($), (<>), (<$>), (>), not)
import SqlToPurs.Model (SQLFunc(SQLFunc), Type(Numeric, Boolean, Int), Var(Out, In))
import Data.String (joinWith)
import Data.Array (replicate)
import Data.List as L
import Data.Foldable (class Foldable, foldMap)

moduleDecl :: String
moduleDecl = joinWith "\n" [ "module MyApp.SQL where"
                           , "import Database.AnyDB (Connection, DB)"
                           , "import Data.Array (Array)"
                           , "import Control.Monad.Aff (Aff)"]

full :: forall f. (Foldable f) => f SQLFunc -> String
full fs = moduleDecl <> "\n" <> foldMap (\s -> typeDecl s <> "\n" <> funcDef s <> "\n") fs

typeDecl :: SQLFunc -> String
typeDecl (SQLFunc {name, vars, set}) = 
  let invars = L.filter isIn vars
      outvars = L.filter (not isIn) vars
   in name 
      <> " :: forall eff. Connection -> " 
      <> varsToPurs invars
      <> (if (L.length invars > 0) then " -> " else "")
      <> "Aff (db :: DB | eff) "
      <> "(" <> (if set then "Array " else "") <> varsToPurs outvars <> ")"

isIn :: Var -> Boolean
isIn (In _ _) = true
isIn _        = false

varsToPurs :: L.List Var -> String
varsToPurs L.Nil = ""
varsToPurs vs = "{" <> joinWith ", " (L.toUnfoldable $ varToPurs <$> vs) <> "}" 

varToPurs :: Var -> String
varToPurs (In name t)  = name <> " :: " <> typeToPurs t
varToPurs (Out name t) = name <> " :: " <> typeToPurs t

typeToPurs :: Type -> String
typeToPurs Int = "Int"
typeToPurs Boolean = "Boolean"
typeToPurs Numeric = "Number"

funcDef :: SQLFunc -> String
funcDef (SQLFunc {name, vars, set}) = 
  let invars = L.filter isIn vars
      outvars = L.filter (not isIn) vars
   in name 
      <> " conn "
      <> (if (L.length invars > 0) 
             then "{" <> (joinWith ", " (L.toUnfoldable $ getName <$> invars)) <> "}" 
             else "")
      <> " = query \"select * from " <> name <> "(" <> toQuestionmarks invars <> ")\" "
      <> "[" <> joinWith ", " (L.toUnfoldable $ (\v -> "toSql " <> getName v) <$> invars) <> "]"
      <> " conn"

getName :: Var -> String
getName (In name _) = name
getName (Out name _) = name

toQuestionmarks :: forall a. L.List a -> String
toQuestionmarks as = joinWith "," $ replicate (L.length as) "?"


{-- myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number}) --}
{-- myfunc conn {myinvar} = query "select * from myfunc(?)" [toSql myinvar] conn --}
