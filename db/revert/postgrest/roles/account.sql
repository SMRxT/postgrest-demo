-- Revert postgrest-demo:postgrest/roles/account from pg

BEGIN;

REVOKE USAGE
   ON SCHEMA postgrest
      FROM postgrest_account;

DROP ROLE postgrest_account;

COMMIT;
