-- Deploy postgrest-demo:postgrest/roles/account to pg
-- requires: postgrest/schema

BEGIN;

CREATE ROLE postgrest_account;

GRANT USAGE 
   ON SCHEMA postgrest
      TO postgrest_account;

COMMIT;
