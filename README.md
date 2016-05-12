Work in progress

Why?
====
PureScript for web applications allows type safety to stretch from frontend to backend. I'd like to also have it stretch to the database. I've had only bad experiences with ORM's. So I wrote this CLI that builds Purescript function definitions from (Postgres) SQL function definitions. The Postgres SQL engine will check the function definitions you feed it (at design time! psql mydb < schema.sql) for syntax and schema errors, and this CLI will then bridge the gap into your PureScript application. It's not a strong guarantee that things will not fail, but with a good build pipeline and/or CI to keep things in sync, this gives (me personally at least) very high confidence that the interactions between my app code and DB code are well typed.

How?
====
Takes a sql file like this: 

```
CREATE FUNCTION myfunc (IN myinvar boolean, OUT myvar numeric(2,2))
RETURNS SETOF record
AS $$
  SELECT myvar
  from invoices
  where amount >= myinvar
  ;
$$ LANGUAGE SQL;
```

and turns it into a purescript file like this:

```
module MyApp.SQL where
import Database.AnyDB (Connection, DB)
import Data.Array (Array)
import Control.Monad.Aff (Aff)
myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})
myfunc conn {myinvar} = query \"select * from myfunc(?)\" [toSql myinvar] conn
```

You can add another 'extra' file (-e or --extra CLI param) which will be inlined straight into the generated .purs file. This comes in handy when using PostgreSQL Data Types that can't be easily mapped to PureScript primitive types.

Data Types that can't be easily mapped to PureScript primitive types
====================================================================
eg: UUID
--------
Postgres's UUID type is used a lot, but Sqltopurs doesn't assume how you want to use the UUID type. It will generate postgres uuid parameters as a UUID type in resulting purescript function. After that, it's up to you how you want to handle it. 
If you want to use it as a newtype, you can add following line to your "extra" file:
```
newtype UUID = UUID String
```

Alternatively you can use the purescript-uuid package, and import this via the "extra"file:
```
import Data.UUID (UUID)
```

You can even use it as just a type alias:
```
type UUID = String
```
