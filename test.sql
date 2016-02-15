CREATE FUNCTION myfunc (IN myinvar boolean, OUT myvar numeric(2,2))
RETURNS SETOF record
AS $$
  SELECT id, enrollment_id, amount, paid
  from invoices
  where amount >= am
  ;
$$ LANGUAGE SQL;

CREATE FUNCTION myfunc (IN myinvar boolean, IN mysecondvar int, OUT myvar numeric(2,2), OUT mysecondoutvar int)
AS $$
  SELECT id, enrollment_id, amount, paid
  from invoices
  where amount >= am
  ;
$$ LANGUAGE SQL;
