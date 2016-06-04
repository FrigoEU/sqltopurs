drop schema public cascade;
create schema public;
GRANT ALL ON SCHEMA public TO public;
COMMENT ON SCHEMA public IS 'standard public schema';
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

create table activities (
  id uuid PRIMARY KEY /* newtype ActivityId */,
  description text NOT NULL
);

create table places (
  id uuid PRIMARY KEY /* newtype PlaceId */,
  description text NOT NULL
);

create table posts (
  id uuid PRIMARY KEY /* newtype PostId */,
  userId uuid NOT NULL /* newtype UserId */,
  activityId uuid NOT NULL /* newtype ActivityId */,
  placeId uuid NOT NULL /* newtype PlaceId */,
  datePoint date, -- Null == Anyday
  timePoint text NOT NULL
);

create table messages (
  id uuid PRIMARY KEY /* newtype MessageId */,
  postId uuid NOT NULL /* newtype PostId */,
  fromUserId uuid NOT NULL /* newtype UserId */,
  toUserId uuid NOT NULL /* newtype UserId */,
  at timestamp with time zone NOT NULL,
  body text NOT NULL
);

 -- TODO encrypt!
create table users (
  id uuid PRIMARY KEY /* newtype UserId */,
  email text UNIQUE NOT NULL,
  password text NOT NULL,
  firstName text NOT NULL,
  lastName text NOT NULL
);

create table sessions (
  id uuid PRIMARY KEY /* newtype SessionId */,
  userId uuid NOT NULL /* newtype UserId */,
  createdAt timestamp with time zone NOT NULL
);

-----------------------------------------------------

CREATE FUNCTION queryAllActivities ()
RETURNS SETOF activities
AS $$
  SELECT * from activities;
$$ LANGUAGE SQL;

CREATE FUNCTION queryAllPlaces ()
RETURNS SETOF places
AS $$
  SELECT * from places;
$$ LANGUAGE SQL;

CREATE FUNCTION queryNewMessages (IN last messages.at%TYPE, IN userId messages.fromUserId%TYPE)
RETURNS SETOF messages
AS $$
  SELECT * from messages
  WHERE fromUserId = userId
    AND at > last;
$$ LANGUAGE SQL;

CREATE FUNCTION queryPostsByActivity (IN activityId posts.activityId%TYPE, OUT id posts.id%TYPE, OUT userId posts.userId%TYPE, OUT activityId posts.activityId%TYPE, OUT placeId posts.placeId%TYPE, OUT datePoint posts.datePoint%TYPE, OUT timePoint posts.timePoint%TYPE, OUT firstName users.firstName%TYPE)
RETURNS SETOF record
AS $$
  SELECT p.*, u.firstName from posts p, users u
  WHERE p.activityId = activityId
    AND u.id = p.userId;
$$ LANGUAGE SQL;

CREATE FUNCTION insertPost (IN userId posts.userId%TYPE, IN activityId posts.activityId%TYPE, IN placeId posts.placeId%TYPE, IN datePoint posts.datePoint%TYPE, IN timePoint posts.timePoint%TYPE, OUT id posts.id%TYPE)
AS $$
  INSERT INTO posts (id, userId, activityId, placeId, datePoint, timePoint)
  VALUES (uuid_generate_v4(), userId, activityId, placeId, datePoint, timePoint)
  RETURNING id
$$ LANGUAGE SQL;

CREATE FUNCTION insertMessage (IN postId messages.postId%TYPE, IN fromUserId messages.fromUserId%TYPE, IN toUserId messages.toUserId%TYPE, IN at messages.at%TYPE, IN body messages.body%TYPE, OUT id messages.id%TYPE)
AS $$
  INSERT INTO messages (id, postId, fromUserId, toUserId, at, body)
  VALUES (uuid_generate_v4(), postId, fromUserId, toUserId, at, body)
  RETURNING id
$$ LANGUAGE SQL;

CREATE FUNCTION dbLogin (IN email users.email%TYPE, IN password users.password%TYPE, IN now sessions.createdAt%TYPE, OUT sessionId sessions.id%TYPE)
AS $$
BEGIN
  IF EXISTS (SELECT * FROM users WHERE email = email AND password = password)
  THEN  INSERT INTO sessions (id, userId, createdAt)
        VALUES (uuid_generate_v4(), userId, now)
        RETURNING id;
  END IF;
END
$$ LANGUAGE plpgsql;

CREATE FUNCTION getSession (IN sessionId sessions.id%TYPE)
RETURNS sessions
AS $$
  SELECT * FROM sessions
  WHERE id = sessionId;
$$ LANGUAGE SQL;

