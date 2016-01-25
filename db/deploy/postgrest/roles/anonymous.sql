-- Deploy postgrest-demo:postgrest/roles/anonymous to pg
-- requires: postgrest/schema

BEGIN;

CREATE ROLE postgrest_anonymous;

GRANT USAGE 
   ON SCHEMA postgrest 
      TO postgrest_anonymous;

COMMIT;
