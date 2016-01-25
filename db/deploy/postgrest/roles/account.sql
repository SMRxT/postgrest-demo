-- Deploy postgrest-demo:postgrest/roles/account to pg
-- requires: postgrest/schema

BEGIN;

CREATE ROLE postgrest_account;

COMMIT;
