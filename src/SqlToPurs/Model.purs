module SqlToPurs.Model where

import Prelude (class Eq, class Show, (==), (&&), show, (<>))
import Data.Generic (class Generic, gShow, gEq)
import Data.List (List)

data Type = Int | Boolean | Numeric | UUID | Text | SqlDate | TimestampWithoutTimeZone
data Var = In String Type
         | Out String Type
data SQLFunc = SQLFunc {name :: String, vars :: List Var, set :: Boolean}

derive instance genericType :: Generic Type
instance showType :: Show Type where show = gShow

derive instance genericVar :: Generic Var
instance showVar :: Show Var where show = gShow
instance eqVar :: Eq Var where eq = gEq

instance showSQLFunc :: Show SQLFunc where 
  show (SQLFunc {name, vars, set}) = "SQLFunc " <> name <> " vars: " <> show vars <> " set: " <> show set

instance eqSQLFunc :: Eq SQLFunc where
  eq (SQLFunc {name: name1, vars: vars1, set: set1}) (SQLFunc {name: name2, vars: vars2, set: set2})
   = name1 == name2 && vars1 == vars2 && set1 == set2
