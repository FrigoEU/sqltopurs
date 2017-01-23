module SqlToPurs.Model where

import Data.Generic (gEq, gShow, class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Prelude (show, class Show, class Eq, (<>))

data Type = Int | Boolean | Numeric | UUID | Text | Date | TimestampWithoutTimeZone | Time
data OutParams = FullTable String
               | Separate (Array Var)
data Var = Var (Maybe String) String String
data TypeAnn = NoAnn | NewType String | Data String
newtype SQLFunc = SQLFunc {name :: String, vars :: {in :: Array Var, out :: OutParams}, set :: Boolean, outers :: Maybe (List String)}
newtype SQLField = SQLField {name :: String, table :: String, type :: Type, primarykey :: Boolean, notnull :: Boolean, newtype :: TypeAnn}
newtype SQLTable = SQLTable {name :: String, fields :: Array SQLField}
newtype NamedField = NamedField {name :: Maybe String, field :: SQLField}
newtype OuterJoined = OuterJoined Boolean

derive instance genericTypeAnn :: Generic TypeAnn
derive instance genericType :: Generic Type
instance showType :: Show Type where show = gShow
instance eqType :: Eq Type where eq = gEq
derive instance genericVar :: Generic Var
instance eqVar :: Eq Var where eq = gEq
derive instance genericOutParams :: Generic OutParams
instance showOutParams :: Show OutParams where show = gShow
instance eqOutParams :: Eq OutParams where eq = gEq
derive instance genericSQLFunc :: Generic SQLFunc
instance showSQLFunc :: Show SQLFunc where show = gShow
instance eqSQLFunc :: Eq SQLFunc where eq = gEq
derive instance genericSQLTable :: Generic SQLTable
instance showSQLTable :: Show SQLTable where show = gShow
instance eqSQLTable :: Eq SQLTable where eq = gEq
derive instance genericSQLField :: Generic SQLField
instance showSQLField :: Show SQLField where show = gShow
instance eqSQLField :: Eq SQLField where eq = gEq
derive instance genericNamedField :: Generic NamedField
instance showNamedField :: Show NamedField where show = gShow
instance eqNamedField :: Eq NamedField where eq = gEq

instance showVar :: Show Var where
  show (Var n table field) = "Var " <> show n <> " " <> table <> "." <> field
