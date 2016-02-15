Work in progress

Why?
====
PureScript for web applications allows type safety to stretch from frontend to backend. I'd like to also have it stretch to the database. I've had only bad experiences with ORM's. So I wrote this CLI that builds Purescript function definitions from (Postgres) SQL function definitions. The Postgres SQL engine will check the function definitions you feed it (at design time!) for syntax and schema errors, and this CLI will then bridge the gap into your PureScript application. It's not a strong guarantee that things will not fail, but with a good build pipeline and/or CI to keep things in sync, this gives (me personally at least) very high confidence that the interactions between my app code and DB code are well typed.

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
