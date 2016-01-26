-- Revert postgrest-demo:postgrest/roles/anonymous from pg

BEGIN;

REVOKE USAGE
   ON SCHEMA postgrest
      FROM postgrest_anonymous;

DROP ROLE postgrest_anonymous;

COMMIT;
