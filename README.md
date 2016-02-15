Work in progress

Why?
====
I've had only bad experiences with ORM's, but still would like to "enforce" type safety between my application code and my database. For every interaction between my app code and the DB I write a function n my sql file(s) and execute them into postgres. Postgres parses and checks the functions against the current schema. When all is well, I generate the purescript files. These functions I can then use in my purescript application, where the compiler will check the type safety further into my application code.

How?
====
Takes a sql file like this: 

```
CREATE FUNCTION myfunc (IN myinvar boolean, OUT myvar numeric(2,2))
RETURNS SETOF record
AS $$
  SELECT id, enrollment_id, amount, paid
  from invoices
  where amount >= am
  ;
$$ LANGUAGE SQL;
```

and turns it into a purescript file like this:

```
myfunc :: forall eff. Connection -> {myinvar :: Boolean} -> Aff (db :: DB | eff) (Array {myvar :: Number})
myfunc conn {myinvar} = query \"select * from myfunc(?)\" [toSql myinvar] conn
```
