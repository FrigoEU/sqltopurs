module SqlToPurs.Model where

import Prelude (class Show, show, (<>))
import Data.Generic (class Generic, gShow)
import Data.List (List)

data Type = Int | Boolean | Numeric
data Var = In String Type
         | Out String Type
data SQLFunc = SQLFunc {name :: String, vars :: List Var, set :: Boolean}

derive instance genericType :: Generic Type
instance showType :: Show Type where show = gShow

derive instance genericVar :: Generic Var
instance showVar :: Show Var where show = gShow

instance showSQLFunc :: Show SQLFunc where 
  show (SQLFunc {name, vars, set}) = "SQLFunc " <> name <> " vars: " <> show vars <> " set: " <> show set
