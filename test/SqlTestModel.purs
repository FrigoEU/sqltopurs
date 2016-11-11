module Test.SqlTestModel where

import Control.Monad.Except (Except, throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (gDecodeJson, decodeJson, class DecodeJson)
import Data.Argonaut.Encode (gEncodeJson, encodeJson, class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)

import Data.Either (either)
import Data.Foreign (Foreign, ForeignError(JSONError, TypeMismatch))
import Data.Generic (class Generic, gShow, gEq)
import Data.List.NonEmpty (NonEmptyList)
import Database.Postgres.SqlValue (SqlValue, fromSql, toSql, class IsSqlValue)
import Prelude (class Eq, class Show, pure, ($), (<$>), (>=>), (>>>))

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

toSqlJson :: forall a. (EncodeJson a) => a -> SqlValue
toSqlJson = encodeJson >>> (printJson :: Json -> String) >>> toSql
fromSqlJson :: forall t67. (DecodeJson t67) => Foreign -> Except (NonEmptyList ForeignError) t67
fromSqlJson = fromSql >=> jsonParserTr >=> decodeJsonTr
  where 
    jsonParserTr str = either (\s -> throwError $ pure $ TypeMismatch s str) pure (jsonParser str)
    decodeJsonTr json = either (\s -> throwError $ pure $ JSONError s) pure (decodeJson json)
