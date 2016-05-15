
blablablablababla;
create table activities (
  id uuid PRIMARY KEY,
  description text NOT NULL
);

create table posts (
  id uuid PRIMARY KEY,
  activityId uuid NOT NULL,
  datePoint date,
  anumber numeric(2,2) NOT NULL
);

CREATE FUNCTION myfunc (IN myinvar activities.id%TYPE)
RETURNS SETOF activities
AS $$
  SELECT id, description
  from activities
  WHERE id = myinvar;
$$ LANGUAGE SQL;

CREATE FUNCTION myfunc2 (IN myinvar posts.id%TYPE, OUT posts.id%TYPE, OUT posts.activityId%TYPE, OUT posts.datePoint%TYPE, OUT posts.anumber%TYPE)
AS $$
SELECT id, activityId, datePoint, anumber
from posts
where id = myinvar;
$$ LANGUAGE SQL;"
