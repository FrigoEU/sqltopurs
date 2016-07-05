drop schema public cascade;
create schema public;
GRANT ALL ON SCHEMA public TO public;
COMMENT ON SCHEMA public IS 'standard public schema';
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

create table testtable (
  id uuid PRIMARY KEY,
  d date NOT NULL,
  twotz timestamp without time zone NOT NULL,
  t time NOT NULL,
  mt time,
  myadt text /* data MyADT */
);

CREATE FUNCTION querytest ()
RETURNS testtable
AS $$
  SELECT * from testtable
  $$ LANGUAGE SQL;

CREATE FUNCTION inserttest (IN d testtable.d%TYPE, IN twotz testtable.twotz%TYPE, IN t testtable.t%TYPE, IN myadt testtable.myadt%TYPE, IN mt testtable.mt%TYPE, OUT id testtable.id%TYPE)
AS $$
  INSERT INTO testtable (id, d, twotz, t, myadt, mt)
  VALUES (uuid_generate_v4(), d, twotz, t, myadt, mt)
  RETURNING id
$$ LANGUAGE SQL;
