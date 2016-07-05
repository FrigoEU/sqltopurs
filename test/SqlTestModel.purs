module Test.SqlTestModel where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (gDecodeJson, decodeJson, class DecodeJson)
import Data.Argonaut.Encode (gEncodeJson, encodeJson, class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (either, Either(Left, Right))
import Data.Foreign (Foreign, ForeignError(JSONError, TypeMismatch))
import Data.Generic (class Generic, gShow, gEq)
import Database.Postgres.SqlValue (SqlValue, fromSql, toSql, class IsSqlValue)
import Prelude (class Show, class Eq, ($), (>=>), (>>>), (<$>))

newtype UUID = UUID String
derive instance genericUUID :: Generic UUID
instance eqUUID :: Eq UUID where eq = gEq
instance showUUID :: Show UUID where show = gShow
instance isSqlValueUUID :: IsSqlValue UUID where
  toSql (UUID s) = toSql s
  fromSql o = UUID <$> fromSql o

data MyADT = One | Two
derive instance genericMyADT :: Generic MyADT
instance eqMyADT :: Eq MyADT where eq = gEq
instance showMyADT :: Show MyADT where show = gShow
instance encodeJsonMyADT :: EncodeJson MyADT where encodeJson = gEncodeJson
instance decodeJsonMyADT :: DecodeJson MyADT where decodeJson = gDecodeJson
instance isSqlValueMyADT :: IsSqlValue MyADT where
  toSql = toSqlJson
  fromSql = fromSqlJson

-- Should this be a constraint on JsonEncode & JsonDecode instead of on Generic?
toSqlJson :: forall a. (EncodeJson a) => a -> SqlValue
toSqlJson = encodeJson >>> (printJson :: Json -> String) >>> toSql
fromSqlJson :: forall a. (DecodeJson a) => Foreign -> Either ForeignError a
fromSqlJson = fromSql >=> jsonParserTr >=> decodeJsonTr
  where 
    jsonParserTr :: String -> Either ForeignError Json
    jsonParserTr str = either (\s -> Left $ TypeMismatch s str) Right (jsonParser str)
    decodeJsonTr :: forall b. (DecodeJson b) => Json -> Either ForeignError b
    decodeJsonTr json = either (\s -> Left $ JSONError s) Right (decodeJson json)
{-- instance isSqlValueMyADT :: IsSqlValue MyADT where --}
{--   toSql One = toSql "one" --}
{--   toSql Two = toSql "two" --}
{--   fromSql str = (fromSql str :: F String) --} 
{--     >>= \s -> case s of --}  
{--                    "one" -> Right One --}
{--                    "two" -> Right Two --}
{--                    _ -> Left (TypeMismatch "Expecting one or two" s) --}
