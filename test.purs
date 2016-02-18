module MyApp.SQL where
import Database.AnyDB (Connection, DB)
import Data.Array (Array)
import Control.Monad.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)
newtype UUID = UUID String

myfunc2 :: forall eff. Connection -> {myinvar :: Boolean, mysecondvar :: Int} -> Aff (db :: DB | eff) ({myvar :: Number, mysecondoutvar :: Int})
myfunc2 conn {myinvar, mysecondvar} = query "select * from myfunc2(?,?)" [toSql myinvar, toSql mysecondvar] conn >>= unsafeCoerce
myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})
myfunc conn {myinvar} = query "select * from myfunc(?)" [toSql myinvar] conn >>= unsafeCoerce
