module MyApp.SQL where
import Prelude ((<$>), return, ($), (>>=), bind)
import Database.AnyDB (Connection, DB)
import Control.Monad.Aff (Aff)
import Data.Foreign.Class (class IsForeign, readProp)
newtype UUID = UUID String

newtype Res0 = Res0 {myvar :: Number, mysecondoutvar :: Int}
runRes0 :: Res0 -> {myvar :: Number, mysecondoutvar :: Int}
runRes0 (Res0 a) = a
instance isForeignRes0 :: IsForeign Res0 where
  read obj = do
    myvar <- readProp "myvar" <$> obj
    mysecondoutvar <- readProp "mysecondoutvar" <$> obj
    return $ Res0 {myvar,mysecondoutvar}
myfunc2 :: forall eff. Connection -> {myinvar :: Boolean, mysecondvar :: Int} -> Aff (db :: DB | eff) ({myvar :: Number, mysecondoutvar :: Int})
myfunc2 conn {myinvar, mysecondvar} = (map runRes0) <$> query "select * from myfunc2(?,?)" [toSql myinvar, toSql mysecondvar] conn

newtype Res1 = Res1 {myvar :: Number}
runRes1 :: Res1 -> {myvar :: Number}
runRes1 (Res1 a) = a
instance isForeignRes1 :: IsForeign Res1 where
  read obj = do
    myvar <- readProp "myvar" <$> obj
    return $ Res1 {myvar}
myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})
myfunc conn {myinvar} = (map runRes1) <$> query "select * from myfunc(?)" [toSql myinvar] conn

